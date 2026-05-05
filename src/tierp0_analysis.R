#' TierP0: Perturbation-Based Power Analysis for NEON Plant Diversity
#'
#' @description
#' Orchestrates perturbation-based power analysis for a single site.
#' Uses PER-SPECIES ADDITIVE perturbation scaled by each species' raw
#' mean cover, and PER-SPECIES ABSOLUTE detection threshold also scaled
#' by raw mean cover. Every species faces the same PROPORTIONAL challenge
#' regardless of baseline cover level.
#'
#' For effect_val = 0.20 (20% relative):
#'   - Species at 10% raw cover gets +2 pp added to predictions
#'   - Species at 60% raw cover gets +12 pp added to predictions
#' For threshold = 0.10 (10% relative):
#'   - Species at 10% raw cover: detection threshold = 1 pp
#'   - Species at 60% raw cover: detection threshold = 6 pp
#'
#' Configuration via environment variables:
#'   - SITE_ID: Site identifier (required)
#'   - INPUT_RDS_PATH: Path to input RDS file (default: /input/plant_data.rds)
#'   - OUTPUT_DIR: Output directory (default: /tmp/outputs)
#'   - EFFECT_SIZES: Comma-separated relative proportions
#'                   (default: "0.20"). Example: "0.10,0.20,0.30,0.50"
#'                   where 0.20 = 20% of each species' raw mean cover
#'   - EFFECT: Single effect size (legacy, overridden by EFFECT_SIZES if set)
#'   - POWER_REPS: Monte Carlo replicates (default: 50)
#'   - POST_DRAWS: Posterior draws to use (default: 1000; max = stored draws)
#'   - MAX_YEARS: Max years to analyze (default: all)
#'   - YEAR_BASELINE: Optional manual year override (single year analysis)
#'
#' Fixed parameters:
#'   - DECISION_RULE: "binary_detect"
#'   - DETECT_THRESHOLD: 0.10 (10% of species' raw mean cover)
#'   - COVER_FLOOR: 3.0% (minimum mean cover for species inclusion)
#'   - N_GRID: c(30, 25, 20, 15, 10, 8, 6, 5)
#'
#' Performance optimization:
#'   Predictions are generated ONCE per year (2 calls to
#'   generate_reduced_predictions), then reused across all effect sizes.
#'
#' @author NEON Optimization Team
#' @date 2025

# ==============================================================================
# Initialization
# ==============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(arrow)
library(gjam)
library(coda)

set.seed(1)
RNGkind("L'Ecuyer-CMRG")

message("========================================")
message("TierP0 Perturbation-Based Power Analysis")
message("========================================")

# ==============================================================================
# 0. Validate Tier Mode
# ==============================================================================

if (tolower(Sys.getenv("TIER_MODE", "tierp0")) != "tierp0") {
  stop("tierp0_analysis.R called with non-TierP0 mode", call. = FALSE)
}

# ==============================================================================
# 1. Configurations
# ==============================================================================

SITE_ID <- Sys.getenv("SITE_ID", "")
if (SITE_ID == "") stop("SITE_ID environment variable must be set")

INPUT_RDS <- Sys.getenv("INPUT_RDS_PATH", "/input/plant_data.rds")
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", "/tmp/outputs")

# Effect sizes as RELATIVE PROPORTIONS (applied per-species as
# raw_cover * effect_val additive perturbation)
effect_sizes_str <- Sys.getenv("EFFECT_SIZES", Sys.getenv("EFFECT", "0.20"))
EFFECT_SIZES <- as.numeric(strsplit(effect_sizes_str, "[,;]")[[1]])

POWER_REPS <- as.integer(Sys.getenv("POWER_REPS", "50"))
POST_DRAWS <- as.integer(Sys.getenv("POST_DRAWS", "1000"))
MAX_YEARS <- as.integer(Sys.getenv("MAX_YEARS", Sys.getenv("MAX_YEAR_PAIRS", "999")))

# Fixed parameters
DECISION_RULE <- "binary_detect"
DETECT_THRESHOLD <- 0.10   # relative: 10% of each species' raw cover
COVER_FLOOR <- 3.0
N_GRID <- c(30, 25, 20, 15, 10, 8, 6, 5)

YEAR_BASELINE <- Sys.getenv("YEAR_BASELINE", "")

message("Site: ", SITE_ID)
message("Effect sizes (relative): ", paste(EFFECT_SIZES * 100, collapse = ", "), "% (", length(EFFECT_SIZES), " values)")
message("Detection threshold: ", DETECT_THRESHOLD * 100, "% of each species' raw cover")
message("Cover floor: ", COVER_FLOOR, "%")
message("Posterior draws: ", POST_DRAWS)
message("Power reps: ", POWER_REPS)
message("Max years: ", if (MAX_YEARS >= 999) "all" else MAX_YEARS)
message("Sample sizes: ", paste(N_GRID, collapse = ", "))
message("Decision rule: ", DECISION_RULE)

# ==============================================================================
# 2. Source All R Function Files
# ==============================================================================

if (!dir.exists("R")) {
  stop("Expected 'R/' directory not found in working directory", call. = FALSE)
}
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f)

for (fn in c("load_neon_data", "fit_gjam_model_test", "normalize_gjam_chains",
             "extract_ygibbs_predictions", "generate_reduced_predictions",
             "run_power_sensitivity")) {
  if (!exists(fn, mode = "function")) {
    stop("Missing required function: ", fn, call. = FALSE)
  }
}
message("  All required functions loaded")

# ==============================================================================
# 3. Load NEON Data
# ==============================================================================

message("\n[1/5] Loading data...")
if (!file.exists(INPUT_RDS)) stop("Input RDS not found at: ", INPUT_RDS)

neon_data <- load_neon_data(INPUT_RDS)
message("  Loaded ", nrow(neon_data), " observations for ", length(unique(neon_data$plotID)), " plots")

# ==============================================================================
# 4. Fit GJAM Model
# ==============================================================================

message("\n[2/5] Fitting GJAM model on full dataset...")
fit_result <- fit_gjam_model_test(neon_data)
fit_result$fit <- normalize_gjam_chains(fit_result$fit)
gc()
message("  Model fit complete")

# Compute raw mean cover per species from original field data (pre-GJAM).
raw_data <- readRDS(INPUT_RDS)
site_raw <- raw_data %>% dplyr::filter(siteID == SITE_ID)
raw_species_cover <- site_raw %>%
  dplyr::group_by(taxonID) %>%
  dplyr::summarise(mean_cover = mean(percentCover, na.rm = TRUE), .groups = "drop") %>%
  tibble::deframe()
message(sprintf("  Raw cover: %d species, range [%.2f, %.2f], %d above %.0f%%",
                length(raw_species_cover),
                min(raw_species_cover), max(raw_species_cover),
                sum(raw_species_cover >= COVER_FLOOR), COVER_FLOOR))
rm(raw_data, site_raw)
gc()

# Determine draw indices (subsample for speed)
n_stored <- nrow(fit_result$fit$chains$bgibbs)
if (POST_DRAWS >= n_stored) {
  draw_idx <- seq_len(n_stored)
} else {
  set.seed(42)  # Deterministic draw selection
  draw_idx <- sort(sample.int(n_stored, size = POST_DRAWS))
}
n_draws_used <- length(draw_idx)
message(sprintf("  Using %d of %d posterior draws", n_draws_used, n_stored))

# ==============================================================================
# 5. Find Eligible Years
# ==============================================================================

message("\n[3/5] Finding eligible years...")
years <- sort(unique(as.character(fit_result$xdata$year)))

if (length(years) < 1) {
  stop("No years of data available for TierP0 analysis")
}

if (YEAR_BASELINE != "") {
  message("  Using manually specified year: ", YEAR_BASELINE)
  n_plots_y <- length(unique(fit_result$xdata$plotID[fit_result$xdata$year == YEAR_BASELINE]))
  if (n_plots_y < 5) stop(sprintf("Year %s has only %d plots (<5).",
                                  YEAR_BASELINE, n_plots_y))
  years_df <- tibble::tibble(year = YEAR_BASELINE, n_plots = n_plots_y)
} else {
  message("  Finding years with >=5 plots...")
  years_df <- tibble::tibble(year = years) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(n_plots = length(unique(fit_result$xdata$plotID[fit_result$xdata$year == year]))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_plots >= 5)
  
  if (nrow(years_df) == 0) stop("No years with >=5 plots.")
  
  # Limit years if requested
  if (MAX_YEARS < nrow(years_df)) {
    message(sprintf("  Limiting to %d of %d years (MAX_YEARS)", MAX_YEARS, nrow(years_df)))
    idx <- round(seq(1, nrow(years_df), length.out = MAX_YEARS))
    years_df <- years_df[idx, ]
  }
}

message("  Found ", nrow(years_df), " eligible years (>=5 plots):")
for (i in 1:nrow(years_df)) {
  message("    ", years_df$year[i], " (", years_df$n_plots[i], " plots)")
}

# ==============================================================================
# 6. Run Power Analysis: Years (outer) × Effect Sizes (inner)
# ==============================================================================
# OPTIMIZATION: Predictions are generated ONCE per year, then reused
# across effect sizes. Perturbation is per-species additive (raw_cover *
# effect_val), applied to the stored prediction array without
# regeneration.

message("\n[4/5] Running power analysis: ", nrow(years_df), " year(s) x ",
        length(EFFECT_SIZES), " effect size(s)...")
message("  [Optimized: predictions generated once per year, reused across effects]")

# Store results indexed by effect size
all_results <- list()
for (ev in EFFECT_SIZES) all_results[[as.character(ev)]] <- list()

xdata <- fit_result$xdata
xdata_indexed <- xdata %>% dplyr::mutate(.row_idx = dplyr::row_number())

for (y_i in 1:nrow(years_df)) {
  year_base <- years_df$year[y_i]
  n_plots_y <- years_df$n_plots[y_i]
  
  message(sprintf("\n  --- Year %d/%d: %s (%d plots) ---",
                  y_i, nrow(years_df), year_base, n_plots_y))
  
  # --------------------------------------------------------------------------
  # 6a. Find plot row indices for this year (no overlap filter; single-year
  #     counterfactual uses all plots sampled this year)
  # --------------------------------------------------------------------------
  
  base_rows <- xdata_indexed %>%
    dplyr::filter(year == year_base) %>%
    dplyr::arrange(plotID)
  base_idx <- base_rows$.row_idx
  plot_ids <- as.character(base_rows$plotID)
  n_plots <- length(plot_ids)
  
  # --------------------------------------------------------------------------
  # 6b. Generate paired predictions ONCE (reused across all effect sizes)
  # --------------------------------------------------------------------------
  
  seed_string <- paste(SITE_ID, year_base, sep = "_")
  seed_base_val <- sum(utf8ToInt(seed_string)) %% 2147483647
  seed_base_pred <- seed_base_val * 2 + 1
  seed_pert_pred <- seed_base_val * 2 + 2
  
  message(sprintf("    Generating predictions: %d draws x %d plots (seeds: %d, %d)",
                  n_draws_used, n_plots, seed_base_pred, seed_pert_pred))
  
  mu_base <- generate_reduced_predictions(
    fit = fit_result$fit, row_indices = base_idx, draws = draw_idx,
    clamp = FALSE, seed = seed_base_pred
  )
  
  mu_pert_raw <- generate_reduced_predictions(
    fit = fit_result$fit, row_indices = base_idx, draws = draw_idx,
    clamp = FALSE, seed = seed_pert_pred
  )
  
  # Align species effect vector to mu_pert_raw's species dimension.
  # Species not present in raw_species_cover get effect = 0 (will be
  # filtered out by cover floor anyway).
  species_names_pred <- dimnames(mu_pert_raw)[[3]]
  cover_vec <- raw_species_cover[species_names_pred]
  cover_vec[is.na(cover_vec)] <- 0
  names(cover_vec) <- species_names_pred
  
  # --------------------------------------------------------------------------
  # 6c. Loop over effect sizes (cheap: per-species additive + power analysis)
  # --------------------------------------------------------------------------
  
  n_grid_augmented <- sort(unique(c(N_GRID[N_GRID <= n_plots], n_plots)), decreasing = TRUE)
  seed_power <- (seed_base_val * 31) %% 2147483647
  
  for (effect_val in EFFECT_SIZES) {
    message(sprintf("    Effect = %.0f%% relative (per-species):", effect_val * 100))
    
    # Per-species additive perturbation: raw_cover * effect_val
    # A species at 10% raw cover with effect_val=0.20 gets +2 pp added
    species_effects <- cover_vec * effect_val
    
    # sweep adds species_effects along the 3rd dimension (species)
    mu_pert <- sweep(mu_pert_raw, 3, species_effects, FUN = "+")
    
    # Package as sim_data for run_power_sensitivity
    sim_data <- list(
      y_base_sim = mu_base,
      y_pert_sim = mu_pert,
      plot_ids = plot_ids,
      meta = list(
        effect = effect_val, effect_type = "per_species_additive",
        year_baseline = year_base,
        n_draws = n_draws_used, n_plots = n_plots
      )
    )
    
    power <- run_power_sensitivity(
      sim_data = sim_data,
      n_grid = n_grid_augmented,
      power_reps = POWER_REPS,
      site_id = SITE_ID,
      threshold = DETECT_THRESHOLD,
      power_threshold = 0.80,
      decision_rule = DECISION_RULE,
      raw_species_cover = raw_species_cover,
      cover_floor = COVER_FLOOR,
      seed = seed_power
    )
    
    # Store with year metadata
    result_entry <- list(
      power_curve = dplyr::mutate(power$power_curve,
                                  year_baseline = year_base,
                                  n_plots_year = n_plots_y, effect = effect_val, .before = 1),
      n_star = dplyr::mutate(power$n_star,
                             year_baseline = year_base,
                             n_plots_year = n_plots_y, effect = effect_val, .before = 1),
      meta = list(year_baseline = year_base,
                  n_plots_year = n_plots_y, effect = effect_val)
    )
    
    all_results[[as.character(effect_val)]][[length(all_results[[as.character(effect_val)]]) + 1]] <- result_entry
    
    rm(mu_pert, species_effects)
  }
  
  # Free prediction arrays before next year
  rm(mu_base, mu_pert_raw, cover_vec)
  gc(verbose = FALSE)
  
  message(sprintf("    Complete for year %s", year_base))
}

message("\n  Power analysis complete for all year pairs and effect sizes")

# ==============================================================================
# 7. Combine and Save Results (Per Effect Size)
# ==============================================================================

message("\n[5/5] Saving results...")

out_dir <- file.path(OUTPUT_DIR, SITE_ID, "tierp0")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (effect_val in EFFECT_SIZES) {
  results <- all_results[[as.character(effect_val)]]
  
  power_curve_all <- dplyr::bind_rows(purrr::map(results, "power_curve")) %>%
    drop_other(taxon_col = "species")
  
  n_star_all <- dplyr::bind_rows(purrr::map(results, "n_star")) %>%
    drop_other(taxon_col = "species")
  
  metadata <- list(
    years = years_df,
    year_results_meta = purrr::map(results, "meta"),
    effect = effect_val,
    effect_type = "per_species_additive",
    all_effect_sizes = EFFECT_SIZES,
    decision_rule = DECISION_RULE,
    detect_threshold = DETECT_THRESHOLD,
    detect_threshold_type = "per_species_relative",
    cover_floor = COVER_FLOOR,
    power_reps = POWER_REPS,
    post_draws = n_draws_used,
    n_grid = N_GRID,
    site_id = SITE_ID,
    convergence_diagnostics = fit_result$fit$convergence_diagnostics,
    timestamp = Sys.time(),
    env_vars = as.list(Sys.getenv()),
    session_info = sessionInfo()
  )
  
  effect_str <- sprintf("%.2f", effect_val)
  
  out_power <- file.path(out_dir, paste0("power_curve_effect", effect_str, ".parquet"))
  arrow::write_parquet(power_curve_all, out_power)
  message("  [OK] Power curve (effect=", effect_val * 100, "%): ", out_power)
  
  out_nstar <- file.path(out_dir, paste0("n_star_power_effect", effect_str, ".parquet"))
  arrow::write_parquet(n_star_all, out_nstar)
  message("  [OK] N* (effect=", effect_val * 100, "%): ", out_nstar)
  
  out_meta <- file.path(out_dir, paste0("metadata_effect", effect_str, ".rds"))
  saveRDS(metadata, out_meta)
  message("  [OK] Metadata (effect=", effect_val * 100, "%): ", out_meta)
}

message("\nAll outputs saved for ", length(EFFECT_SIZES), " effect size(s)")

# ==============================================================================
# 8. Validation
# ==============================================================================

out_files_all <- character()
for (effect_val in EFFECT_SIZES) {
  effect_str <- sprintf("%.2f", effect_val)
  out_files_all <- c(out_files_all,
                     file.path(out_dir, paste0("power_curve_effect", effect_str, ".parquet")),
                     file.path(out_dir, paste0("n_star_power_effect", effect_str, ".parquet")),
                     file.path(out_dir, paste0("metadata_effect", effect_str, ".rds")))
}

missing <- out_files_all[!file.exists(out_files_all)]
if (length(missing) > 0) {
  stop("TierP0 failed: missing output files: ", paste(missing, collapse = ", "), call. = FALSE)
}

empty_files <- out_files_all[file.info(out_files_all)$size == 0]
if (length(empty_files) > 0) {
  stop("TierP0 failed: empty output files: ", paste(empty_files, collapse = ", "), call. = FALSE)
}

message("\n========================================")
message("TierP0 Analysis Complete!")
message("========================================")
message("Effect sizes tested (relative): ", paste(EFFECT_SIZES * 100, collapse = ", "), "%")
message("Detection threshold: ", DETECT_THRESHOLD * 100, "% of each species' raw cover")
message("Posterior draws used: ", n_draws_used)
message("Years analyzed: ", nrow(years_df))
message("Total output files: ", length(out_files_all))
message("Total N* rows: ", nrow(n_star_all))
message("Output directory: ", out_dir)
message("========================================")
