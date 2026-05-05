# _targets.R — Tier1 and Tier2A analysis pipeline
# NOTE: TierP0 and TrendRun now use dedicated scripts (tierp0_analysis.R, trendrun_analysis.R)
#       and bypass the targets framework entirely
library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "dplyr","tidyr","purrr","stringr","ggplot2","glue",
    "readr","tibble","rmarkdown","arrow","rlang","gjam","coda"
  ),
  format = "rds",
  memory = "transient",
  garbage_collection = TRUE,
  workspace_on_error = TRUE
)

# Quick/slow knobs
.quick <- identical(tolower(Sys.getenv("GJAM_QUICK","false")), "true")
.SS    <- if (.quick) c(2,5,10) else c(2,3,4,5,6,8,10,15,20,25)
.REP   <- if (.quick) 1L else 3L

# Site identifier (required)
SITE_ID <- Sys.getenv("SITE_ID", "")
if (SITE_ID == "") stop("SITE_ID environment variable must be set")

# Tier mode control - diagnostic logging
message("[_targets.R] TIER_MODE = '", Sys.getenv("TIER_MODE"), "'")
.TIER_MODE <- tolower(Sys.getenv("TIER_MODE", "tier1"))  # "tier1" | "tier2a" only (tierp0/trendrun use separate scripts)
message("[_targets.R] Parsed .TIER_MODE = '", .TIER_MODE, "'")

# Validate recognized mode
if (!.TIER_MODE %in% c("tier1", "tier2a")) {
  stop(sprintf("Invalid TIER_MODE='%s'. _targets.R only supports 'tier1' or 'tier2a'. Use tierp0_analysis.R or trendrun_analysis.R for those modes.", .TIER_MODE))
}

.TIER2_REPS <- as.integer(Sys.getenv("TIER2_REPS", "25"))  # B replicates for Tier 2A
.TIER2_SAMPLE_FRAC <- as.numeric(Sys.getenv("TIER2_SAMPLE_FRAC", "0.8"))

# Parse N_GRID safely (supports "2:15" or "2,3,4,5")
.N_GRID <- tryCatch(
  eval(parse(text = Sys.getenv("N_GRID", "2:15"))),
  error = function(e) {
    # Fallback: try comma-separated
    tryCatch(
      as.integer(strsplit(Sys.getenv("N_GRID", "2,3,4,5,6,8,10,15"), ",")[[1]]),
      error = function(e2) stop("N_GRID must be R expression like '2:15' or comma-separated like '2,3,4,5'")
    )
  }
)

.NOISE_MODE <- Sys.getenv("NOISE_MODE", "sigma")             # "sigma" or "mu_only"
.DECISION_RULE <- Sys.getenv("DECISION_RULE", "posterior_prob")  # "posterior_prob" or "binary_detect"

# Source all R helpers (you already created schema_* and normalizer files)
targets::tar_source("R")

# Output directory
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = "outputs")
ensure_dir <- function(path) { if (!dir.exists(path)) dir.create(path, recursive = TRUE); path }

# TierP0 targets (LEGACY - always empty list since tierp0 mode no longer uses targets framework)
# TODO: Remove this entire ~600-line tierp0_targets definition in future cleanup
tierp0_targets <- if (FALSE) list(  # Changed from .TIER_MODE == "tierp0" to FALSE
  tar_target(
    plant_data_file,
    {
      path <- Sys.getenv("INPUT_RDS_PATH", "/input/plant_data.rds")
      if (!file.exists(path)) stop(sprintf("Input RDS not found at: %s", path), call. = FALSE)
      path
    },
    format = "file"
  ),

  # Prep tables used upstream and later written to Parquet
  tar_target(relative_cover_df, compute_relative_cover(plant_data_file)),
  tar_target(neon_data,         load_neon_data(plant_data_file)),

  # --- Fit once per site and prune (unchanged logic) ---
  tar_target(
    fit_result,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      x <- fit_gjam_model_test(neon_data)
      x$fit <- normalize_gjam_chains(x$fit)  # your helper
      gc(); x
    }
  ),
  tar_target(
    fit_result_lean,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      mode <- tolower(Sys.getenv("PRUNE_MODE","conservative"))
      if (!mode %in% c("conservative","aggressive")) mode <- "conservative"
      prune_fit_result(fit_result, mode = mode)
    }
  ),
  
  tar_target(
    xnew_in_sample,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      fit_result_lean$xdata
    }
  ),

  # Optional posterior draws (unused by report directly)
  tar_target(
    posterior_draws,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      manual_posterior_predict(fit = fit_result_lean$fit, xnew = xnew_in_sample)
    }
  ),

  # --- Sensitivity blocks (baseline + variable) ---
  tar_target(
    sensitivity_results_baseline,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      run_baseline_full_available(fit_result = fit_result_lean, seed = 123)
    }
  ),
  tar_target(
    sensitivity_results_variable,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      run_sample_size_sensitivity_variable(
        fit_result   = fit_result_lean,
        sample_sizes = .SS,
        n_replicates = .REP,
        seed         = 123
      )
    }
  ),

  # --- Species-level summaries: bind + standardize + validate + write Parquet ---
  tar_target(
    species_summary_combined,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      dplyr::bind_rows(
        dplyr::mutate(sensitivity_results_variable$summary, mode = "sensitivity_variable"),
        dplyr::mutate(sensitivity_results_baseline$summary, mode = "baseline")
      )
    }
  ),
  tar_target(
    species_summary_std,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      normalize_species_summary(species_summary_combined)
    }
  ),
  tar_target(
    validate_species,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      validate_species_summary(species_summary_std)
    }
  ),
  tar_target(
    species_summary_file,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      ensure_dir(OUTPUT_DIR)
      out <- file.path(OUTPUT_DIR, "species_summary.parquet")
      arrow::write_parquet(drop_other(species_summary_std, "species"), out)
      out
    },
    format = "file"
  ),

  # --- Community-weighted detection (variable mode) ---
  tar_target(
    community_detection,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      evaluate_community_weighted_detection(
        sensitivity_results = sensitivity_results_variable$summary,
        relative_cover_df   = relative_cover_df,
        draws_df            = sensitivity_results_variable$draws
      )
    }
  ),
  tar_target(
    community_detection_file,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      ensure_dir(OUTPUT_DIR)
      out <- file.path(OUTPUT_DIR, "community_detection.parquet")
      arrow::write_parquet(community_detection, out)
      out
    },
    format = "file"
  ),

  # --- Community-weighted detection (baseline mode) ---
  tar_target(
    community_detection_baseline,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      evaluate_community_weighted_detection(
        sensitivity_results = sensitivity_results_baseline$summary,
        relative_cover_df   = relative_cover_df,
        draws_df            = sensitivity_results_baseline$draws
      )
    }
  ),
  tar_target(
    community_detection_baseline_file,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      ensure_dir(OUTPUT_DIR)
      out <- file.path(OUTPUT_DIR, "community_detection_baseline.parquet")
      arrow::write_parquet(community_detection_baseline, out)
      out
    },
    format = "file"
  ),

  # --- Draws index (kept as index with paths if present): bind + write Parquet ---
  tar_target(
    draws_index_combined,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      di_var <- dplyr::mutate(sensitivity_results_variable$draws, mode = "sensitivity_variable")
      di_base <- dplyr::mutate(sensitivity_results_baseline$draws, mode = "baseline")
      di <- dplyr::bind_rows(di_var, di_base)
      # If a list/Path column named 'file' exists, coerce to character for Parquet
      if ("file" %in% names(di)) di$file <- as.character(di$file)
      di
    }
  ),
  tar_target(
    draws_index_file,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      ensure_dir(OUTPUT_DIR)
      out <- file.path(OUTPUT_DIR, "draws_index.parquet")
      arrow::write_parquet(draws_index_combined, out)
      out
    },
    format = "file"
  ),

  # --- Relative cover: validate + write Parquet ---
  tar_target(
    rel_cover_valid,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      validate_relative_cover(relative_cover_df)
    }
  ),
  tar_target(
    rel_cover_file,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      ensure_dir(OUTPUT_DIR)
      out <- file.path(OUTPUT_DIR, "relative_cover.parquet")
      arrow::write_parquet(rel_cover_valid, out)
      out
    },
    format = "file"
  ),

  # --- Schema audit for species summary ---
  tar_target(
    schema_report,
    {
      if (.TIER_MODE != "tier1") return(NULL)
      ensure_dir(OUTPUT_DIR)
      rpt <- schema_audit(species_summary_std, "species_summary")
      out <- file.path(OUTPUT_DIR, "schema_report.csv")
      readr::write_csv(rpt, out)
      out
    },
    format = "file"
  ),

  # --- TIER 2A: Refit-based uncertainty (gated by TIER_MODE) ---
  # Only runs if TIER_MODE = "tier2a"
  tar_target(
    tier2_site_data,
    {
      if (.TIER_MODE != "tier2a") return(NULL)
      
      site_id <- unique(neon_data$siteID)
      
      # Build Tier 2A inputs directly from neon_data (no Tier 1 dependency)
      y_wide <- neon_data %>%
        tidyr::pivot_wider(
          id_cols = c("siteID", "plotID", "year", "nlcdClass"),
          names_from = taxonID,
          values_from = mean_cover,
          values_fill = 0
        )
      
      xdata <- y_wide %>%
        dplyr::select(siteID, plotID, year, nlcdClass)
      
      # ydata needs plotID and year for resample_plots() row matching
      ydata <- y_wide %>%
        dplyr::select(-siteID, -nlcdClass)
      
      # Get species columns (exclude plotID, year which are kept in ydata for resampling)
      species_cols <- setdiff(names(ydata), c("plotID", "year"))
      
      # Apply deterministic species filtering (same rules as Tier 1)
      # Remove all-zero columns
      col_sums <- colSums(ydata[, species_cols, drop = FALSE], na.rm = TRUE)
      keep_species <- names(col_sums)[col_sums > 0]
      if (length(keep_species) < length(species_cols)) {
        message("Tier2A: Dropping ", length(species_cols) - length(keep_species), " all-zero species")
        ydata <- ydata[, c("plotID", "year", keep_species), drop = FALSE]
        species_cols <- keep_species
      }
      
      # Remove zero-variance columns
      col_vars <- apply(ydata[, species_cols, drop = FALSE], 2, var, na.rm = TRUE)
      keep_species <- names(col_vars)[col_vars > 0]
      if (length(keep_species) < length(species_cols)) {
        message("Tier2A: Dropping ", length(species_cols) - length(keep_species), " zero-variance species")
        ydata <- ydata[, c("plotID", "year", keep_species), drop = FALSE]
        species_cols <- keep_species
      }
      
      # Cheap contract checks (always-on anti-drift protection)
      stopifnot("x/y row count mismatch" = nrow(xdata) == nrow(ydata))
      stopifnot("Missing required x variables" = all(c("siteID","plotID","year","nlcdClass") %in% names(xdata)))
      stopifnot("No species retained after filtering" = length(species_cols) > 0)
      
      # Log diagnostics
      years <- sort(unique(xdata$year))
      message(sprintf(
        "[Tier2A] Inputs: site=%s, n_plots=%d, n_plot_years=%d, n_years=%d, year_range=%s-%s, n_species=%d",
        site_id, length(unique(xdata$plotID)), nrow(xdata), length(years),
        min(years), max(years), length(species_cols)
      ))
      
      # Package data needed for Tier 2 refits
      list(
        xdata = xdata,
        ydata = ydata,
        site_id = site_id,
        modelList = list(
          ng = 5000,
          burnin = 2500,
          typeNames = "CA",
          REDUCT = FALSE
        )
      )
    }
  ),
  
  tar_target(
    tier2_replicates,
    {
      if (.TIER_MODE != "tier2a") return(NULL)
      if (is.null(tier2_site_data)) return(NULL)
      
      # Run B replicates (each is a complete refit)
      purrr::map(seq_len(.TIER2_REPS), function(rep_id) {
        run_tier2_replicate(
          site_data = tier2_site_data,
          replicate_id = rep_id,
          seed_base = 123,
          sample_frac = .TIER2_SAMPLE_FRAC,
          sample_sizes = .SS,
          n_sensitivity_reps = .REP
        )
      })
    }
  ),
  
  tar_target(
    n_star_tier2,
    {
      if (.TIER_MODE != "tier2a") return(NULL)
      if (is.null(tier2_replicates)) return(NULL)
      
      summarize_tier2_nstar(tier2_replicates)
    }
  ),
  
  # Replicate-level summary (operational metadata)
  tar_target(
    tier2_replicates_summary,
    {
      if (.TIER_MODE != "tier2a") return(NULL)
      if (is.null(tier2_replicates)) return(NULL)
      
      # Extract summary from each replicate (fit time, status, hash, etc.)
      dplyr::bind_rows(lapply(tier2_replicates, function(x) x$summary))
    }
  ),
  
  # Detailed replicate-level data for diagnostics
  tar_target(
    tier2_replicates_detailed,
    {
      if (.TIER_MODE != "tier2a") return(NULL)
      if (is.null(tier2_replicates)) return(NULL)
      
      # Extract and combine n_star_details from all replicates
      dplyr::bind_rows(lapply(tier2_replicates, function(x) x$n_star_details)) %>%
        drop_other("species")
    }
  ),
  
  tar_target(
    tier2_output_file,
    {
      if (.TIER_MODE != "tier2a" || is.null(n_star_tier2)) return(NULL)
      
      ensure_dir(OUTPUT_DIR)
      tier2_dir <- file.path(OUTPUT_DIR, "tier2")
      ensure_dir(tier2_dir)
      
      out <- file.path(tier2_dir, "n_star_tier2.parquet")
      arrow::write_parquet(n_star_tier2, out)
      message(sprintf("✓ Tier 2A N* written: %s", out))
      out
    },
    format = "file"
  ),
  
  # Save detailed replicate data
  tar_target(
    tier2_replicates_file,
    {
      if (.TIER_MODE != "tier2a" || is.null(tier2_replicates_detailed)) return(NULL)
      
      ensure_dir(OUTPUT_DIR)
      tier2_dir <- file.path(OUTPUT_DIR, "tier2")
      ensure_dir(tier2_dir)
      
      out <- file.path(tier2_dir, "replicates_tier2.parquet")
      arrow::write_parquet(tier2_replicates_detailed, out)
      message(sprintf("✓ Tier 2A replicate details written: %s", out))
      out
    },
    format = "file"
  ),
  
  # Save replicate-level summary (operational metadata)
  tar_target(
    tier2_replicates_summary_file,
    {
      if (.TIER_MODE != "tier2a" || is.null(tier2_replicates_summary)) return(NULL)
      
      ensure_dir(OUTPUT_DIR)
      tier2_dir <- file.path(OUTPUT_DIR, "tier2")
      ensure_dir(tier2_dir)
      
      out <- file.path(tier2_dir, "replicate_summary_tier2.parquet")
      arrow::write_parquet(tier2_replicates_summary, out)
      message(sprintf("✓ Tier 2A replicate summary written: %s", out))
      out
    },
    format = "file"
  )
) else list()  # NOTE: This first tierp0_targets definition is immediately overwritten by the second one below

# TierP0 targets (LEGACY - always empty list since tierp0 mode no longer uses targets framework)
# TODO: Remove this entire ~200-line tierp0_targets definition in future cleanup
tierp0_targets <- if (FALSE) list(  # Changed from .TIER_MODE == "tierp0" to FALSE
  tar_target(
    tierp0_fit,
    {
      message("[TierP0] Fitting model on full dataset...")
      x <- fit_gjam_model_test(neon_data)
      x$fit <- normalize_gjam_chains(x$fit)
      gc()
      x
    }
  ),
    
    # Find all eligible year pairs for perturbation analysis
    tar_target(
      tierp0_year_pairs,
      {
        years <- sort(unique(as.character(tierp0_fit$xdata$year)))
        
        if (length(years) < 2) {
          stop("Need at least 2 years of data for TierP0 perturbation analysis")
        }
        
        # env override (optional)
        yb <- Sys.getenv("YEAR_BASELINE", "")
        yp <- Sys.getenv("YEAR_PERTURBED", "")
        if (yb != "" && yp != "") {
          p1 <- unique(tierp0_fit$xdata$plotID[tierp0_fit$xdata$year == yb])
          p2 <- unique(tierp0_fit$xdata$plotID[tierp0_fit$xdata$year == yp])
          ov <- length(intersect(p1, p2))
          if (ov < 5) {
            stop(sprintf("Specified year pair %s→%s has only %d overlapping plots (<5).", yb, yp, ov))
          }
          message(sprintf("[TierP0] Using specified year pair: %s → %s (overlap=%d)", yb, yp, ov))
          return(tibble::tibble(year_base = yb, year_pert = yp, overlap = ov))
        }
        
        # Find all consecutive pairs
        pairs <- tibble::tibble(
          year_base = years[-length(years)],
          year_pert = years[-1]
        )
        
        # Compute overlap for each pair
        pairs <- dplyr::rowwise(pairs) %>%
          dplyr::mutate(
            overlap = {
              p1 <- unique(tierp0_fit$xdata$plotID[tierp0_fit$xdata$year == year_base])
              p2 <- unique(tierp0_fit$xdata$plotID[tierp0_fit$xdata$year == year_pert])
              length(intersect(p1, p2))
            }
          ) %>%
          dplyr::ungroup() %>%
          dplyr::filter(overlap >= 5)
        
        if (nrow(pairs) == 0) {
          stop("No consecutive year pairs with >=5 plots overlap.")
        }
        
        message(sprintf("[TierP0] Found %d eligible year pairs (overlap >=5): %s",
                        nrow(pairs),
                        paste(sprintf("%s→%s(%d)", pairs$year_base, pairs$year_pert, pairs$overlap),
                              collapse = ", ")))
        pairs
      }
    ),
    
    # Run power analysis for each year pair
    tar_target(
      tierp0_pair_results,
      {
        message(sprintf("[TierP0] Running power analysis for %d year pairs...", nrow(tierp0_year_pairs)))
        
        purrr::pmap(
          tierp0_year_pairs,
          function(year_base, year_pert, overlap) {
            message(sprintf("  Processing %s → %s (%d plots)...", year_base, year_pert, overlap))
            
            # Simulate data
            sim <- simulate_perturbed_dataset(
              fit_result = tierp0_fit,
              year_baseline = year_base,
              year_perturbed = year_pert,
              effect = .EFFECT,
              noise_mode = .NOISE_MODE,
              seed = 12345
            )
            
            # Run power analysis
            power <- run_power_sensitivity(
              sim_data = sim,
              n_grid = .N_GRID,
              power_reps = .POWER_REPS,
              site_id = SITE_ID,
              threshold = 0.20,
              power_threshold = 0.80,
              decision_rule = .DECISION_RULE,
              seed = 67890
            )
            
            # Add year columns to outputs
            list(
              power_curve = dplyr::mutate(power$power_curve,
                                          year_baseline = year_base,
                                          year_perturbed = year_pert,
                                          n_plots_overlap = overlap,
                                          .before = 1),
              n_star = dplyr::mutate(power$n_star,
                                     year_baseline = year_base,
                                     year_perturbed = year_pert,
                                     n_plots_overlap = overlap,
                                     .before = 1),
              meta = list(year_baseline = year_base, 
                          year_perturbed = year_pert, 
                          n_plots_overlap = overlap)
            )
          }
        )
      }
    ),
    
    # Combine power curves from all year pairs
    tar_target(
      tierp0_power_curve_all,
      dplyr::bind_rows(purrr::map(tierp0_pair_results, "power_curve"))
    ),
    
    # Combine N* results from all year pairs
    tar_target(
      tierp0_n_star_all,
      dplyr::bind_rows(purrr::map(tierp0_pair_results, "n_star"))
    ),
    
    # Save power curve to file
    tar_target(
      tierp0_power_curve_file,
      {
        ensure_dir(OUTPUT_DIR)
        tierp0_dir <- file.path(OUTPUT_DIR, SITE_ID, "tierp0")
        ensure_dir(tierp0_dir)
        
        out <- file.path(tierp0_dir, "power_curve.parquet")
        arrow::write_parquet(tierp0_power_curve_all, out)
        message(sprintf("✓ TierP0 power curve written: %s (%d year pairs)", 
                        out, length(unique(tierp0_power_curve_all$year_baseline))))
        out
      },
      format = "file"
    ),
    
    # Save N* results to file
    tar_target(
      tierp0_n_star_file,
      {
        tierp0_dir <- file.path(OUTPUT_DIR, SITE_ID, "tierp0")
        ensure_dir(tierp0_dir)
        out <- file.path(tierp0_dir, "n_star_power.parquet")
        arrow::write_parquet(tierp0_n_star_all, out)
        message(sprintf("✓ TierP0 N* written: %s (%d year pairs)", 
                        out, length(unique(tierp0_n_star_all$year_baseline))))
        out
      },
      format = "file"
    ),
    
    # Save metadata
    tar_target(
      tierp0_meta_file,
      {
        tierp0_dir <- file.path(OUTPUT_DIR, SITE_ID, "tierp0")
        ensure_dir(tierp0_dir)
        meta <- list(
          year_pairs = tierp0_year_pairs,
          pair_results_meta = purrr::map(tierp0_pair_results, "meta"),
          effect = .EFFECT,
          noise_mode = .NOISE_MODE,
          decision_rule = .DECISION_RULE,
          power_reps = .POWER_REPS,
          n_grid = .N_GRID,
          tier_mode = .TIER_MODE
        )
        out <- file.path(tierp0_dir, "metadata.rds")
        saveRDS(meta, out)
        message(sprintf("✓ TierP0 metadata written: %s", out))
        out
      },
      format = "file"
    )
) else list()

# Combine main targets with TierP0 targets
c(
  list(

  # --- Inputs ---
  tar_target(
    plant_data_file,
    {
      path <- Sys.getenv("INPUT_RDS_PATH", "/input/plant_data.rds")
      if (!file.exists(path)) stop(sprintf("Input RDS not found at: %s", path), call. = FALSE)
      path
    },
    format = "file"
  ),

  # Prep tables used upstream and later written to Parquet
  tar_target(relative_cover_df, compute_relative_cover(plant_data_file)),
  tar_target(neon_data,         load_neon_data(plant_data_file)),

  # ... all the Tier1/Tier2A targets ...
  
  tar_target(
    tier2_replicates_summary_file,
    {
      if (.TIER_MODE != "tier2a" || is.null(tier2_replicates_summary)) return(NULL)
      
      ensure_dir(OUTPUT_DIR)
      tier2_dir <- file.path(OUTPUT_DIR, "tier2")
      ensure_dir(tier2_dir)
      
      out <- file.path(tier2_dir, "replicate_summary_tier2.parquet")
      arrow::write_parquet(tier2_replicates_summary, out)
      message(sprintf("✓ Tier 2A replicate summary written: %s", out))
      out
    }
  )
  ),
  tierp0_targets
)
