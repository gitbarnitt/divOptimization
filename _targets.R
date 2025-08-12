# _targets.R
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr", "tidyr", "purrr", "stringr", "ggplot2", "glue",
    "readr", "tibble", "rmarkdown", "arrow"
  ),
  format = "rds"
)

# ---- quick/slow knobs (env-driven) ----
.quick <- identical(tolower(Sys.getenv("GJAM_QUICK", "false")), "true")
.SS    <- if (.quick) c(5, 10) else c(5, 10, 15, 20, 25)
.REP   <- if (.quick) 1L       else 3L

# Auto-source every R file in the R/ folder (your functions live there)
targets::tar_source("R")

# Ensure outputs/ exists before we render
make_outputs_dir <- function(path = "outputs") {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

list(
  # Raw data path (file target)
  tar_target(
    plant_data_file,
    "data/plant_data.rds",
    format = "file"
  ),
  
  # Read the RDS into memory
  tar_target(
    raw_data,
    readRDS(plant_data_file)
  ),
  
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
  
  # Check function and file names (ensure R/check_names.R defines check_pipeline_symbols())
  tar_target(
    check_names,
    {
      check_pipeline_symbols(
        full_detection_summary          = full_detection_summary,
        community_detection             = community_detection,
        full_detection_summary_baseline = if (exists("community_detection_baseline")) full_detection_summary_baseline else NULL,
        community_detection_baseline    = if (exists("community_detection_baseline")) community_detection_baseline else NULL
      )
    }
  ),
  
  # 1) Fit once
  tar_target(
    fit_result,
    fit_gjam_model_test(neon_data)
  ),
  
  # 2) Normalize chain names (bgibbs/sigErrGibbs -> betaBeta/sigmaSave)
  tar_target(
    test_result_norm,
    {
      x <- fit_result
      x$fit <- normalize_gjam_chains(x$fit)
      x
    }
  ),
  
  # 3) (Optional) In-sample xnew for smoke tests
  tar_target(
    xnew_in_sample,
    test_result_norm$xdata
  ),
  
  # 4) (Optional) Posterior draws from the single fit
  tar_target(
    posterior_draws,
    manual_posterior_predict(
      fit  = test_result_norm$fit,
      xnew = xnew_in_sample
    )
  ),
  
  # 5) Sensitivity analysis — uses the normalized fit (no refits)
  tar_target(
    sensitivity_results,
    run_sample_size_sensitivity(
      fit_result   = test_result_norm,
      sample_sizes = c(5, 10, 15, 20, 25),
      n_replicates = 3,
      seed         = 123
    )
  ),
  
  # Baseline: full available per pair (Option 1)
  tar_target(
    sensitivity_results_baseline,
    run_baseline_full_available(
      fit_result = test_result_norm,
      seed       = 123
    )
  ),
  
  # Variable per-pair curves: k = min(K, n_av(pair)) (Option 2)
  tar_target(
    sensitivity_results_variable,
    run_sample_size_sensitivity_variable(
      fit_result   = test_result_norm,
      sample_sizes = .SS, #c(5, 10, 15, 20, 25),
      n_replicates = .REP,  #3,
      seed         = 123
    )
  ),
  
  # Species-level summary with uncertainty (VARIABLE mode as default)
  tar_target(
    full_detection_summary,
    summarize_species_detection_with_uncertainty(
      summary_df = sensitivity_results_variable$summary,
      draws_df   = read_draws_index(sensitivity_results_variable$draws)
    )
  ),
  
  # Community-weighted mean with CIs (VARIABLE mode as default)
  tar_target(
    community_detection,
    evaluate_community_weighted_detection(
      sensitivity_results = sensitivity_results_variable$summary,
      relative_cover_df   = relative_cover_df,
      draws_df            = read_draws_index(sensitivity_results_variable$draws)
    )
  ),
  
  # Variable mode summaries (reads Parquet draws on demand)
  tar_target(
    full_detection_summary_variable,
    summarize_species_detection_with_uncertainty(
      summary_df = sensitivity_results_variable$summary,
      draws_df   = read_draws_index(sensitivity_results_variable$draws)
    )
  ),
  
  tar_target(
    community_detection_variable,
    evaluate_community_weighted_detection(
      sensitivity_results = sensitivity_results_variable$summary,
      relative_cover_df   = relative_cover_df,
      draws_df            = read_draws_index(sensitivity_results_variable$draws)
    )
  ),
  
  # Baseline mode summaries
  tar_target(
    full_detection_summary_baseline,
    summarize_species_detection_with_uncertainty(
      summary_df = sensitivity_results_baseline$summary,
      draws_df   = read_draws_index(sensitivity_results_baseline$draws)
    )
  ),
  
  tar_target(
    community_detection_baseline,
    evaluate_community_weighted_detection(
      sensitivity_results = sensitivity_results_baseline$summary,
      relative_cover_df   = relative_cover_df,
      draws_df            = read_draws_index(sensitivity_results_baseline$draws)
    )
  ),
  
  
  # Make sure outputs/ exists
  tar_target(
    outputs_dir,
    make_outputs_dir("outputs")
  ),
  
  tar_target(
    detection_report,
    {
      # Touch deps so targets tracks them
      outputs_dir
      community_detection
      full_detection_summary
      relative_cover_df
      full_detection_summary_baseline
      community_detection_baseline
      sensitivity_results_variable
      sensitivity_results_baseline
      test_result_norm
      
      env <- new.env(parent = globalenv())
      # Variable mode (default in Rmd)
      env$community_detection             <- community_detection
      env$full_detection_summary          <- full_detection_summary
      env$relative_cover_df               <- relative_cover_df
      
      # Baseline mode overlays
      env$full_detection_summary_baseline <- full_detection_summary_baseline
      env$community_detection_baseline    <- community_detection_baseline
      
      # Draws index tables so Rmd can read Parquet lazily when needed
      env$draws_idx_variable              <- sensitivity_results_variable$draws
      env$draws_idx_baseline              <- sensitivity_results_baseline$draws
      
      # Basic fit info for descriptive reporting
      env$fit_info                        <- test_result_norm
      
      rmarkdown::render(
        input       = "detection_report.Rmd",
        output_file = "outputs/detection_report.html",
        params      = list(example_site = "JERC", top_n_species = 6),
        envir       = env,
        quiet       = TRUE
      )
      "outputs/detection_report.html"
    },
    format = "file"
  )
)