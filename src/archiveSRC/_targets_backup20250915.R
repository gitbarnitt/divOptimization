# _targets.R
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr", "tidyr", "purrr", "stringr", "ggplot2", "glue",
    "readr", "tibble", "rmarkdown", "arrow", "rlang",
    "tidyselect", "gjam"                           # <<< CHANGED (optional safety for Rmd chunk)
  ),
  format = "rds"
)

tar_option_set(
  memory = "transient",          # don't cache large targets in RAM
  garbage_collection = TRUE,     # force gc() between targets
  workspace_on_error = TRUE      # save workspace on target error for debugging
)

# ---- quick/slow knobs (env-driven) ----
.quick <- identical(tolower(Sys.getenv("GJAM_QUICK", "false")), "true")
.SS    <- if (.quick) c(5, 10) else c(5, 10, 15, 20, 25)
.REP   <- if (.quick) 1L       else 3L

# Auto-source every R file in the R/ folder (your functions live there)
targets::tar_source("R")

# Ensure outputs/ exists before render
make_outputs_dir <- function(path = Sys.getenv("OUTPUT_DIR", "/tmp/outputs")) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

list(
  # --- Input file path from env (Cloud Run-compatible) ---
  tar_target(
    plant_data_file,
    {
      path <- Sys.getenv("INPUT_RDS_PATH", "/input/plant_data.rds")
      if (!file.exists(path)) stop(sprintf("Input RDS not found at: %s", path), call. = FALSE)
      path
    },
    format = "file"
  ),
  
  # Read the RDS into memory
  #tar_target(
  #  raw_data,
  #  readRDS(plant_data_file)
  #),
  
  # Relative cover from file path (function expects a path)
  tar_target(
    relative_cover_df,
    compute_relative_cover(plant_data_file)
  ),
  
  # Model-ready data (function expects a path)
  tar_target(
    neon_data,
    load_neon_data(plant_data_file)
  ),
  
  # Sanity check
  tar_target(
    check_names,
    {
      check_pipeline_symbols(
        full_detection_summary          = full_detection_summary,
        community_detection             = community_detection,
        full_detection_summary_baseline = if (exists("full_detection_summary_baseline")) full_detection_summary_baseline else NULL,
        community_detection_baseline    = if (exists("community_detection_baseline")) community_detection_baseline else NULL
      )
    }
  ),
  
  # 1) Fit once
  tar_target(
    fit_result,
    {
      x <- fit_gjam_model_test(neon_data)
      x$fit <- normalize_gjam_chains(x$fit)
      gc()
      x
    },
    memory = "transient"
  ),
  
  # 2) Memory-lean version for downstream use
  tar_target(
    fit_result_lean,
    {
      mode <- tolower(Sys.getenv("PRUNE_MODE", "conservative"))
      if (!mode %in% c("conservative","aggressive")) mode <- "conservative"
      prune_fit_result(fit_result, mode = mode)
    },
    memory = "transient"
  ),
  
  tar_target(
    xnew_in_sample,
    fit_result_lean$xdata
  ),
  
  # 4) Optional posterior draws (not required by report; will not run unless depended on)
  tar_target(
    posterior_draws,
    manual_posterior_predict(
      fit  = fit_result_lean$fit,
      xnew = xnew_in_sample
    ),
    memory = "transient"
  ),
  
  # 5) Sensitivity analysis (no refits)
  tar_target(
    sensitivity_results,
    run_sample_size_sensitivity(
      fit_result   = fit_result_lean,
      sample_sizes = .SS,
      n_replicates = .REP,
      seed         = 123
    )
  ),
  
  # Baseline: full available per pair
  tar_target(
    sensitivity_results_baseline,
    run_baseline_full_available(
      fit_result = fit_result_lean,
      seed       = 123
    )
  ),
  
  # Variable per-pair curves: k = min(K, n_av(pair))
  tar_target(
    sensitivity_results_variable,
    run_sample_size_sensitivity_variable(
      fit_result   = fit_result_lean,
      sample_sizes = .SS,
      n_replicates = .REP,
      seed         = 123
    )
  ),
  
  tar_target(
    draws_contract_variable,
    validate_draws_contract(sensitivity_results_variable$draws),
    memory = "transient"
  ),
  tar_target(
    draws_contract_baseline,
    validate_draws_contract(sensitivity_results_baseline$draws),
    memory = "transient"
  ),
  
  # --- Summaries (VARIABLE) ---
  # Stream draws via index to keep memory low
  tar_target(
    full_detection_summary,
    { draws_contract_variable
      summarize_species_detection_with_uncertainty(
        summary_df = sensitivity_results_variable$summary,
        draws_df   = sensitivity_results_variable$draws
      )
    }
  ),
  
  tar_target(
    community_detection,
    { draws_contract_variable
      evaluate_community_weighted_detection(
        sensitivity_results = sensitivity_results_variable$summary,
        relative_cover_df   = relative_cover_df,
        draws_df            = sensitivity_results_variable$draws
      )
    }
  ),
  
  # --- Summaries (BASELINE) ---
  tar_target(
    full_detection_summary_baseline,
    { draws_contract_baseline                     # <<< change here
      summarize_species_detection_with_uncertainty(
        summary_df = sensitivity_results_baseline$summary,
        draws_df   = sensitivity_results_baseline$draws
      )
    }
  ),
  
  tar_target(
    community_detection_baseline,
    { draws_contract_baseline                     # <<< and gate this as well
      evaluate_community_weighted_detection(
        sensitivity_results = sensitivity_results_baseline$summary,
        relative_cover_df   = relative_cover_df,
        draws_df            = sensitivity_results_baseline$draws
      )
    }
  ),
  
  # --- Render report ---
  tar_target(
    outputs_dir,
    make_outputs_dir(Sys.getenv("OUTPUT_DIR", "/tmp/outputs"))
  ),
  
  tar_target(
    detection_report,
    {
      # Touch deps
      outputs_dir
      community_detection
      full_detection_summary
      relative_cover_df
      full_detection_summary_baseline
      community_detection_baseline
      sensitivity_results_variable
      sensitivity_results_baseline
      
      out_dir <- Sys.getenv("OUTPUT_DIR", "/tmp/outputs")
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
      message("[detection_report] OUTPUT_DIR = ", out_dir)
      
      env <- new.env(parent = globalenv())
      # Variable mode (default in Rmd)
      env$community_detection             <- community_detection
      env$full_detection_summary          <- full_detection_summary
      env$relative_cover_df               <- relative_cover_df
      
      # Baseline overlays
      env$full_detection_summary_baseline <- full_detection_summary_baseline
      env$community_detection_baseline    <- community_detection_baseline
      
      # Draws index tables so Rmd can read Parquet lazily when needed
      env$draws_idx_variable              <- sensitivity_results_variable$draws
      env$draws_idx_baseline              <- sensitivity_results_baseline$draws
      
      # Basic fit info
      env$fit_info                        <- fit_result_lean
      
      rmarkdown::render(
        input         = "detection_report.Rmd",
        output_file   = "detection_report.html",
        output_format = "html_document",
        output_dir    = out_dir,
        params        = list(example_site = "JERC", top_n_species = 6),
        envir         = env,
        quiet         = TRUE
      )
      file.path(out_dir, "detection_report.html")
    },
    format = "file"
  )
)
