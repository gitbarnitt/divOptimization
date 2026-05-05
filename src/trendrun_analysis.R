#' TrendRun: Multi-Year Trend Detection Power Analysis for NEON Plant Diversity
#'
#' @description
#' Orchestrates moving-window trend detection analysis for a single site.
#' Evaluates whether directional multi-year trends are detectable under
#' varying sample sizes (number of plots).
#'
#' Performs two sensitivity analyses:
#'   - Sensitivity A (SensA): Detectability of the existing/background trend
#'     in the data as sample size changes. Answers: "given what's actually
#'     happening at this site, does our design have sufficient power?"
#'   - Sensitivity B (SensB): Detectability of an imposed trend of known
#'     magnitude as sample size changes. Answers: "if a trend of this rate
#'     were present, would we detect it?" (classical power analysis)
#'
#' @details
#' **Design rationale:**
#'
#' This analysis is distinct from TierP0 (year-pair contrasts). TierP0 asks
#' whether a single year-to-year change is detectable; TrendRun asks whether
#' a sustained directional signal across multiple years is detectable. These
#' are fundamentally different statistical problems — year-to-year variability
#' that makes pairwise change easy to detect simultaneously makes trends hard
#' to detect by masking the underlying slope.
#'
#' **Moving windows (Urquhart et al. 1998):**
#'
#' Trend detection is evaluated over all possible W-year windows within the
#' site's record (e.g., for W=5 and years 2014-2024: windows [2014-2018],
#' [2015-2019], ..., [2020-2024]). This provides estimates of how detectability
#' varies over time and uses all available data rather than privileging the
#' most recent period.
#'
#' **Plot subsampling:**
#'
#' Within each window, K plots are drawn INDEPENDENTLY for each year from
#' that year's available plots. Different years may use different specific
#' plots. This matches a monitoring design where the same number of plots
#' are sampled each year but not necessarily the same plots — avoiding the
#' severe plot-continuity constraints imposed by requiring identical plots
#' across all years (which collapses testable K at sites with COVID-era
#' gaps). The window is eligible if every year has at least 3 plots, and
#' K is limited to the minimum per-year plot count within the window.
#'
#' **Detection rule:**
#'
#' A trend is detected if Pr(|slope| > slope_threshold) >= 0.80 across
#' posterior draws. The slope_threshold is fixed at 0.20 / time_span for
#' both SensA and SensB, matching the 20% cumulative change criterion used
#' in TierP0. In SensB, TREND_SIZES controls only the magnitude of the
#' imposed signal, not the detection criterion. Here time_span is the actual
#' calendar year span (max(years) - min(years)) for each window, accounting 
#' for any gaps in the time series.
#'
#' Configuration via environment variables:
#'   - SITE_ID: Site identifier (required)
#'   - INPUT_RDS_PATH: Path to input RDS file (default: "/input/plant_data.rds")
#'   - OUTPUT_DIR: Output directory (default: "/tmp/outputs")
#'   - POST_DRAWS: Number of posterior draws to use (default: 1000)
#'   - TREND_SIZES: Comma-separated trend magnitudes, e.g., "0.05,0.10,0.20" (default: "0.20")
#'   - TREND_REPS: Monte Carlo replicates for trend analysis (default: 100) 
#'
#' Outputs (Parquet files in <OUTPUT_DIR>/<SITE_ID>/trendrun/):
#'   - trend_meta.parquet: Observation-level metadata
#'   - baseline_trend_species.parquet: Full-record species-level slopes
#'   - sensA_trend_species.parquet: Baseline detectability by window and sample size
#'   - sensA_trend_community.parquet: Community-level SensA summaries
#'   - sensA_nstar.parquet: Minimum sample size for baseline trend detection
#'   - sensB_trend_species_trend<X>.parquet: Imposed trend detectability (one per TREND_SIZES value)
#'   - sensB_trend_community_trend<X>.parquet: Community-level SensB summaries (one per TREND_SIZES value)
#'   - sensB_nstar_trend<X>.parquet: Minimum sample size for imposed trend detection (one per TREND_SIZES value)
#'
#' @references
#' Urquhart, N.S., Paulsen, S.G., & Larsen, D.P. (1998). Monitoring for
#'   policy-relevant regional trends over time. Ecological Applications, 8(2).
#' Clark, J.S., Nemergut, D., Seyednasrollah, B., Turner, P.J., & Zhang, S.
#'   (2017). Generalized joint attribute modeling for biodiversity analysis.
#'   Methods in Ecology and Evolution, 8(4).
#'
#' @author NEON Optimization Team
#' @date 2025

# ==============================================================================
# Helper Functions: Deterministic Seeding
# ==============================================================================

#' Convert numeric value to safe 32-bit integer seed
seed_i32 <- function(x) {
  m <- as.numeric(.Machine$integer.max)
  x <- as.numeric(x)
  if (!is.finite(x)) return(1L)
  as.integer(((x %% m) + m) %% m) + 1L
}

#' Generate deterministic integer seed from string
site_seed <- function(x) {
  vals <- utf8ToInt(x)
  if (length(vals) == 0) return(1L)
  m <- as.numeric(.Machine$integer.max)
  acc <- 0.0
  for (i in seq_along(vals)) {
    acc <- (acc + (as.numeric(vals[i]) * i)) %% m
  }
  seed_i32(acc)
}

# ==============================================================================
# Helper Functions: Moving Windows and Slope Estimation
# ==============================================================================

#' Generate all moving windows of width W from a sorted year vector
#'
#' @param years_all Integer vector of available years (sorted)
#' @param W Integer window width in years
#' @return List of window specifications, each containing:
#'   years_win, window_start, window_end, time_raw, t (centered), W_used
#'   Returns NULL if insufficient years.
make_moving_windows <- function(years_all, W) {
  if (length(years_all) < W || W < 3) return(NULL)

  n_windows <- length(years_all) - W + 1
  lapply(seq_len(n_windows), function(i) {
    yrs <- years_all[i:(i + W - 1)]
    time_raw <- yrs - min(yrs)  # Use actual calendar year spacing
    time_span <- max(yrs) - min(yrs)  # Actual time span
    t_centered <- time_raw - mean(time_raw)
    list(
      window_id    = i,
      window_start = yrs[1],
      window_end   = yrs[W],
      years_win    = yrs,
      time_raw     = time_raw,
      time_span    = time_span,
      t            = t_centered,
      W_used       = W
    )
  })
}

#' OLS slope from centered time index (NA-safe)
#'
#' Computes beta = sum(t*y) / sum(t^2) which is the OLS slope when t is
#' mean-centered. No centering of y is required (proof: sum(t*ybar) = ybar*sum(t) = 0).
#'
#' @param y Numeric vector of annual mean abundances
#' @param t Numeric vector of mean-centered time indices (same length as y)
#' @return Scalar slope, or NA if fewer than 3 finite observations
slope_one <- function(y, t) {
  ok <- is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  sum(t[ok] * y[ok]) / sum(t[ok]^2)
}

# ==============================================================================
# Core Function: Trend Detection Power Analysis for a Single Window
# ==============================================================================

#' Run trend detection sensitivity analysis for one moving window
#'
#' Shared logic for both SensA (baseline) and SensB (imposed trend).
#' Subsamples K plots (same plots across all years in window), computes
#' annual means, optionally adds an imposed trend, fits slopes per
#' posterior draw, and evaluates detection probability.
#'
#' @param pred 3D array [draws, observations, species] of posterior predictions
#' @param plots_by_year Named list: plots available per year
#' @param row_ids_by_plot_year Named list: row indices per "year.plotID" key
#' @param window_spec List from make_moving_windows() for one window
#' @param n_plots_grid Integer vector of sample sizes to test
#' @param trend_reps Integer number of Monte Carlo replicates
#' @param slope_threshold Numeric minimum |slope| for detection
#' @param impose_trend Logical: add imposed trend? (SensB=TRUE, SensA=FALSE)
#' @param beta_step Numeric per-year trend increment (only used if impose_trend=TRUE)
#' @param site_id Character site identifier
#' @param base_seed Numeric base seed for this window (deterministic)
#'
#' @return Tibble with columns: siteID, window_start, window_end, window_years,
#'   sample_size, n_eligible_plots, species, detect_prob, n_reps, n_draws.
#'   Returns NULL if fewer than 3 plots are eligible.
run_window_trend_detection <- function(
    pred,
    plots_by_year,
    row_ids_by_plot_year,
    window_spec,
    n_plots_grid,
    trend_reps,
    slope_threshold,
    impose_trend = FALSE,
    beta_step = 0,
    site_id,
    base_seed,
    return_slopes = FALSE
) {
  years_win <- window_spec$years_win
  t_win     <- window_spec$t
  time_raw  <- window_spec$time_raw
  W_used    <- window_spec$W_used

  n_draw <- dim(pred)[1]
  n_sp   <- dim(pred)[3]
  species_names <- dimnames(pred)[[3]]

  # --------------------------------------------------------------------------
  # Find plots available per year in this window and determine eligibility.
  # Independent-plots design: each year can use different plots. The window
  # is eligible if every year has at least 3 plots. K is limited to the
  # minimum per-year count so that K plots can be drawn from every year.
  # --------------------------------------------------------------------------
  plots_per_window_year <- lapply(years_win, function(yr) {
    plots_by_year[[as.character(yr)]]
  })
  names(plots_per_window_year) <- as.character(years_win)
  
  plots_per_year_count <- vapply(plots_per_window_year, length, integer(1))
  min_plots_any_year <- min(plots_per_year_count)
  
  if (min_plots_any_year < 3) {
    years_str <- paste(years_win, collapse = ",")
    message(sprintf("  Window [%s]: %d points over %d years, min plots/year=%d (<3), skipping",
                    years_str, W_used, window_spec$time_span, min_plots_any_year))
    return(NULL)
  }

  # n_eligible_plots = minimum across years in this window (determines max testable K)
  n_eligible <- min_plots_any_year
  n_plots_grid <- sort(unique(c(n_plots_grid[n_plots_grid <= n_eligible], n_eligible)), decreasing = TRUE)

  # --------------------------------------------------------------------------
  # OPTIMIZATION: Precompute per-plot prediction means for each year.
  # This moves the expensive pred[, idx, ] lookup OUT of the rep loop.
  # Result: plot_means_by_year[[yr]] is a [n_draw x n_plots_yr x n_sp] array
  # where each plot's subplots (if any) are already averaged.
  # The rep loop then only indexes into these small precomputed arrays.
  # --------------------------------------------------------------------------
  plot_means_by_year <- list()
  for (yr in years_win) {
    yr_char <- as.character(yr)
    yr_plots <- plots_per_window_year[[yr_char]]
    n_yr_plots <- length(yr_plots)
    
    yr_means <- array(NA_real_, dim = c(n_draw, n_yr_plots, n_sp),
                      dimnames = list(NULL, yr_plots, species_names))
    
    for (p_i in seq_len(n_yr_plots)) {
      idx <- row_ids_by_plot_year[[paste(yr, yr_plots[p_i], sep = ".")]]
      if (length(idx) == 1) {
        yr_means[, p_i, ] <- pred[, idx, ]
      } else if (length(idx) > 1) {
        yr_means[, p_i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
      }
    }
    plot_means_by_year[[yr_char]] <- yr_means
  }

  # Precompute sum(t^2) for slope calculation (constant across reps)
  t_ss <- sum(t_win^2)
  
  results_list <- list()

  for (K in n_plots_grid) {
    if (K > min_plots_any_year) {
      next
    }

    detect_mat <- matrix(NA_real_, nrow = trend_reps, ncol = n_sp,
                         dimnames = list(NULL, species_names))
    
    # Slope statistics: store median slope per rep (across posterior draws)
    # to measure how stable the slope estimate is across plot resamples
    slope_median_mat <- if (return_slopes) {
      matrix(NA_real_, nrow = trend_reps, ncol = n_sp,
             dimnames = list(NULL, species_names))
    } else NULL

    for (rep in seq_len(trend_reps)) {
      # Compute annual means: for each year, sample K plots from precomputed
      # per-plot means. No access to the large pred array in this loop.
      mu_year <- array(NA_real_, dim = c(n_draw, W_used, n_sp))

      for (i in seq_along(years_win)) {
        yr_char <- as.character(years_win[i])
        yr_means <- plot_means_by_year[[yr_char]]
        n_yr_plots <- dim(yr_means)[2]
        
        set.seed(seed_i32(base_seed + 1000 * K + rep * 100 + i))
        sampled_idx <- sample.int(n_yr_plots, K, replace = FALSE)
        
        # Average across K sampled plots: [n_draw x K x n_sp] -> [n_draw x n_sp]
        if (K == 1) {
          mu_year[, i, ] <- yr_means[, sampled_idx, ]
        } else {
          mu_year[, i, ] <- apply(yr_means[, sampled_idx, , drop = FALSE], c(1, 3), mean)
        }
      }

      # Optionally impose additive trend on annual means
      if (impose_trend) {
        mu_year <- sweep(mu_year, 2, beta_step * time_raw, FUN = "+")
      }

      # Vectorized slope computation across all draws and species:
      # slope = sum(t * y) / sum(t^2) for centered t.
      # mu_year is [n_draw x W_used x n_sp], t_win is length W_used.
      # Multiply each year-slice by its t value, sum across years.
      slopes <- matrix(0, nrow = n_draw, ncol = n_sp)
      for (i in seq_len(W_used)) {
        slopes <- slopes + t_win[i] * mu_year[, i, ]
      }
      slopes <- slopes / t_ss

      # Detection: posterior probability that |slope| exceeds threshold.
      detect_mat[rep, ] <- colMeans(abs(slopes) > slope_threshold, na.rm = TRUE)
      
      # Store mean slope across posterior draws for this rep
      if (return_slopes) {
        slope_median_mat[rep, ] <- colMeans(slopes, na.rm = TRUE)
      }
    }

    # Power = mean detection probability across Monte Carlo replicates
    out_tibble <- tibble::tibble(
      siteID           = site_id,
      window_start     = as.integer(years_win[1]),
      window_end       = as.integer(tail(years_win, 1)),
      window_years     = W_used,
      time_span        = window_spec$time_span,
      sample_size      = K,
      n_eligible_plots = min_plots_any_year,
      species          = species_names,
      detect_prob      = colMeans(detect_mat, na.rm = TRUE),
      n_reps           = trend_reps,
      n_draws          = n_draw
    )
    
    # Add slope stability metrics if requested
    if (return_slopes) {
      out_tibble$slope_mean <- colMeans(slope_median_mat, na.rm = TRUE)
      out_tibble$slope_sd   <- apply(slope_median_mat, 2, sd, na.rm = TRUE)
      out_tibble$slope_cv   <- ifelse(
        abs(out_tibble$slope_mean) > 1e-10,
        out_tibble$slope_sd / abs(out_tibble$slope_mean),
        NA_real_
      )
    }
    
    results_list[[length(results_list) + 1]] <- out_tibble
  }

  if (length(results_list) == 0) return(NULL)
  dplyr::bind_rows(results_list)
}

# ==============================================================================
# Main Script: TrendRun Orchestration
# ==============================================================================

library(dplyr)
library(arrow)
library(tibble)
library(jsonlite)
library(gjam)
library(coda)

# Initialize RNG
set.seed(1)
RNGkind("L'Ecuyer-CMRG")

message("========================================")
message("TrendRun: Multi-Year Trend Detection")
message("========================================")

# ==============================================================================
# 1. Configuration
# ==============================================================================

# Validate tier mode
if (tolower(Sys.getenv("TIER_MODE", "trendrun")) != "trendrun") {
  stop("trendrun_analysis.R called with non-trendrun mode", call. = FALSE)
}

SITE_ID    <- Sys.getenv("SITE_ID", "")
if (SITE_ID == "") stop("SITE_ID environment variable must be set")

INPUT_RDS  <- Sys.getenv("INPUT_RDS_PATH", "/input/plant_data.rds")
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", "/tmp/outputs")
POST_DRAWS <- as.integer(Sys.getenv("POST_DRAWS", "1000"))

# Trend analysis parameters
# Parse trend sizes (comma-separated list or single value)
trend_sizes_str <- Sys.getenv("TREND_SIZES", Sys.getenv("TREND_ADD_NET", "0.20"))
TREND_SIZES <- as.numeric(strsplit(trend_sizes_str, "[,;]")[[1]])
TREND_REPS    <- as.integer(Sys.getenv("TREND_REPS", "50"))

# Moving window widths (years)
TREND_WINDOWS <- c(10L, 5L)

# Sample size ladder (same as TierP0 for comparability)
N_PLOTS_GRID <- c(30, 25, 20, 15, 10, 8, 6, 5)

# Detection threshold: 0.80 posterior probability that |slope| exceeds threshold
DETECT_CUTOFF <- 0.80

OUT_DIR <- file.path(OUTPUT_DIR, SITE_ID, "trendrun")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("Site: ", SITE_ID)
message("Imposed net trends: ", paste(TREND_SIZES, collapse = ", "), " (", length(TREND_SIZES), " values)")
message("Trend reps: ", TREND_REPS)
message("Window widths: ", paste(TREND_WINDOWS, collapse = ", "), " years")
message("Sample sizes: ", paste(N_PLOTS_GRID, collapse = ", "))
message("Detection cutoff: ", DETECT_CUTOFF)

# ==============================================================================
# 2. Write Run Metadata
# ==============================================================================

run_config <- list(
  site_id        = SITE_ID,
  input_rds      = INPUT_RDS,
  out_dir        = OUT_DIR,
  post_draws     = POST_DRAWS,
  trend_sizes    = TREND_SIZES,
  trend_reps     = TREND_REPS,
  trend_windows  = TREND_WINDOWS,
  n_plots_grid   = N_PLOTS_GRID,
  detect_cutoff  = DETECT_CUTOFF,
  timestamp      = as.character(Sys.time())
)
jsonlite::write_json(run_config, file.path(OUT_DIR, "run_metadata.json"),
                     pretty = TRUE, auto_unbox = TRUE)

# ==============================================================================
# 3. Source R Function Files
# ==============================================================================

r_dir <- if (dir.exists("R")) "R" else if (dir.exists("src/R")) "src/R" else
  stop("Neither 'R/' nor 'src/R/' directory found", call. = FALSE)

r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f)

for (fn in c("load_neon_data", "fit_gjam_model_test",
             "extract_ygibbs_predictions", "drop_other")) {
  if (!exists(fn, mode = "function")) {
    stop("Missing required function: ", fn, call. = FALSE)
  }
}
message("  All required functions loaded")

# ==============================================================================
# 4. Load Data
# ==============================================================================

message("\n[1/7] Loading data...")
if (!file.exists(INPUT_RDS)) stop("Input RDS not found at: ", INPUT_RDS)

neon_data <- load_neon_data(INPUT_RDS)
message("  Loaded ", nrow(neon_data), " observations for ",
        dplyr::n_distinct(neon_data$plotID), " plots")

# ==============================================================================
# 5. Fit GJAM Model
# ==============================================================================

message("\n[2/7] Fitting GJAM model...")
fit_result <- fit_gjam_model_test(neon_data)
fit_result$fit <- normalize_gjam_chains(fit_result$fit)
gc()
message("  Model fit complete")

# ==============================================================================
# 6. Prepare Metadata and Generate Predictions
# ==============================================================================

message("\n[3/7] Generating posterior predictions with observation-level noise...")

xdata_trend <- fit_result$xdata %>%
  dplyr::mutate(
    row_id   = dplyr::row_number(),
    year_num = as.integer(as.character(year))
  )

trend_meta <- xdata_trend %>%
  dplyr::select(row_id, plotID, year_num, nlcdClass)
trend_meta$key <- paste(trend_meta$year_num, trend_meta$plotID, sep = ".")

arrow::write_parquet(trend_meta, file.path(OUT_DIR, "trend_meta.parquet"))

year_counts <- trend_meta %>%
  dplyr::count(year_num, name = "n_rows") %>%
  dplyr::arrange(year_num)
arrow::write_parquet(year_counts, file.path(OUT_DIR, "year_counts.parquet"))

# Subsample posterior draws if requested
all_draws <- nrow(fit_result$fit$chains$bgibbs)

if (!is.na(POST_DRAWS) && POST_DRAWS > 0 && POST_DRAWS < all_draws) {
  set.seed(site_seed(SITE_ID))
  draw_idx <- sort(sample.int(all_draws, POST_DRAWS))
} else {
  draw_idx <- NULL
}

pred <- extract_ygibbs_predictions(
  fit_result$fit,
  draws = draw_idx,
  clamp = FALSE
)

stopifnot(length(dim(pred)) == 3)
stopifnot(dim(pred)[2] == nrow(xdata_trend))

years  <- sort(unique(trend_meta$year_num))
n_draw <- dim(pred)[1]
n_sp   <- dim(pred)[3]

message("  Predictions generated: ", n_draw, " draws × ",
        dim(pred)[2], " observations × ", n_sp, " species")

arrow::write_parquet(tibble::tibble(year = years),
                     file.path(OUT_DIR, "years_available.parquet"))

# ==============================================================================
# 7. Baseline Slopes (Full Record, All Plots)
# ==============================================================================
#
# Provides a reference summary: "what trend is present using ALL available
# data?" This is NOT subsampled — it represents the best estimate of the
# actual trend at this site. SensA below evaluates how this trend's
# detectability changes as plots are reduced.
# ==============================================================================

message("\n[4/7] Computing baseline slopes (full record)...")

mu_year_full <- array(NA_real_, dim = c(n_draw, length(years), n_sp),
                      dimnames = list(NULL, as.character(years), dimnames(pred)[[3]]))
for (i in seq_along(years)) {
  idx <- which(trend_meta$year_num == years[i])
  mu_year_full[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
}

t_full <- as.numeric(years) - mean(as.numeric(years))

baseline_slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
for (d in seq_len(n_draw)) {
  baseline_slopes[d, ] <- apply(mu_year_full[d, , ], 2, slope_one, t = t_full)
}

species_names <- dimnames(pred)[[3]]
baseline_species_summary <- tibble::tibble(
  siteID       = SITE_ID,
  species      = species_names,
  slope_median = apply(baseline_slopes, 2, stats::median, na.rm = TRUE),
  slope_lo     = apply(baseline_slopes, 2, stats::quantile, probs = 0.025, na.rm = TRUE),
  slope_hi     = apply(baseline_slopes, 2, stats::quantile, probs = 0.975, na.rm = TRUE),
  p_slope_pos  = colMeans(baseline_slopes > 0, na.rm = TRUE),
  n_years      = length(years)
)
arrow::write_parquet(drop_other(baseline_species_summary, "species"),
                     file.path(OUT_DIR, "baseline_trend_species.parquet"))
message("  Baseline slopes complete (", length(years), " years)")

# ==============================================================================
# 8. Filter Sparse Years for Window Construction
# ==============================================================================
#
# Remove years with too few plots sampled (e.g., COVID-2020). These years are
# excluded from moving window analysis to avoid biasing trend estimates, but
# all data contributed to the GJAM model fit and baseline slopes above.
# ==============================================================================

MIN_PLOTS_PER_YEAR <- 5

year_plot_counts <- trend_meta %>%
  dplyr::distinct(plotID, year_num) %>%
  dplyr::count(year_num, name = "n_plots")

sparse_years <- year_plot_counts %>%
  dplyr::filter(n_plots < MIN_PLOTS_PER_YEAR) %>%
  dplyr::pull(year_num)

if (length(sparse_years) > 0) {
  message(sprintf("\n[TrendRun] Excluding %d sparse year(s) from windows: %s (<%d plots sampled)",
                  length(sparse_years), paste(sparse_years, collapse = ", "), MIN_PLOTS_PER_YEAR))
  years_for_windows <- setdiff(years, sparse_years)
} else {
  years_for_windows <- years
}

message(sprintf("[TrendRun] Using %d years for window construction: %s\n",
                length(years_for_windows), paste(years_for_windows, collapse = ", ")))

# ==============================================================================
# 9. Precompute Mappings for Efficient Resampling
# ==============================================================================

plots_by_year <- split(trend_meta$plotID, trend_meta$year_num)
plots_by_year <- lapply(plots_by_year, unique)
row_ids_by_plot_year <- split(trend_meta$row_id, trend_meta$key)

# ==============================================================================
# 10. Sensitivity B: Imposed Trend Detectability (Moving Windows)
# ==============================================================================
#
# For each trend magnitude in TREND_SIZES and window width W, an additive trend
# of trend_magnitude / time_span per year is imposed on the annual means before
# computing slopes, where time_span is the actual calendar year span for each window.
# Detection asks: Pr(|slope| > slope_threshold) across posterior draws, where
# slope_threshold = 0.20 / time_span (fixed, matching SensA) — the minimum per-year
# rate we consider ecologically meaningful. The trend_magnitude controls only the
# signal imposed on the data, not the detection criterion.
#
# Loop over TREND_SIZES (outer loop - avoids refitting GJAM or regenerating predictions)
# ==============================================================================

message("\n[5/7] Sensitivity B: imposed trend detectability...")
message("  Testing ", length(TREND_SIZES), " trend magnitude(s): ", paste(TREND_SIZES, collapse = ", "))

# Store results per trend magnitude
sensB_all_by_trend <- list()

for (trend_magnitude in TREND_SIZES) {
  message("\n  === Trend magnitude: ", trend_magnitude, " (+", trend_magnitude * 100, "% net) ===")
  
  sensB_all <- list()

  for (W in TREND_WINDOWS) {
    windows <- make_moving_windows(years_for_windows, W)
    if (is.null(windows)) {
      message("    Skipping W=", W, " (insufficient years)")
      next
    }

    # Show example window format for clarity
    ex_win <- windows[[1]]
    ex_years <- paste(ex_win$years_win, collapse = ",")
    message(sprintf("    W=%d: %d windows (e.g., [%s]: %d points over %d years)",
                    W, length(windows), ex_years, ex_win$W_used, ex_win$time_span))

    for (win in windows) {
      # Per-year slope increment to impose (signal) and detection threshold (criterion)
      beta_step       <- trend_magnitude / win$time_span        # imposed signal varies with TREND_SIZES
      slope_threshold <- 0.20 / win$time_span                    # detection threshold fixed at 0.20 (same as SensA)

      base_seed <- as.numeric(site_seed(SITE_ID)) +
        10000 * W + 100 * win$window_id

      result <- run_window_trend_detection(
        pred                 = pred,
        plots_by_year        = plots_by_year,
        row_ids_by_plot_year = row_ids_by_plot_year,
        window_spec          = win,
        n_plots_grid         = N_PLOTS_GRID,
        trend_reps           = TREND_REPS,
        slope_threshold      = slope_threshold,
        impose_trend         = TRUE,
        beta_step            = beta_step,
        site_id              = SITE_ID,
        base_seed            = base_seed
      )

      if (!is.null(result)) {
        result$scenario <- paste0("net", signif(trend_magnitude, 3), "_over_", W, "yr")
        sensB_all[[length(sensB_all) + 1]] <- result
      }
    }
  }
  
  # Combine results for this trend magnitude and add trend_magnitude column
  sensB_trend_species <- dplyr::bind_rows(sensB_all) %>%
    dplyr::mutate(trend_magnitude = trend_magnitude, .before = 1) %>%
    drop_other("species")
  
  # Format trend magnitude for filename (e.g., 0.05 -> "0.05", 0.2 -> "0.20")
  trend_str <- sprintf("%.2f", trend_magnitude)
  
  # Write separate files per trend magnitude
  arrow::write_parquet(sensB_trend_species,
                       file.path(OUT_DIR, paste0("sensB_trend_species_trend", trend_str, ".parquet")))
  message("    SensB complete for trend=", trend_magnitude, ": ", nrow(sensB_trend_species), " rows")

  # ===========================================================================
  # Community Summaries and N* for this trend magnitude  
  # ===========================================================================

  sensB_trend_community <- sensB_trend_species %>%
    dplyr::group_by(siteID, scenario, window_years, window_start, window_end, sample_size) %>%
    dplyr::summarise(
      frac_detectable   = mean(detect_prob >= DETECT_CUTOFF, na.rm = TRUE),
      median_detect_prob = median(detect_prob, na.rm = TRUE),
      n_species         = dplyr::n(),
      .groups = "drop"
    )
  arrow::write_parquet(sensB_trend_community,
                       file.path(OUT_DIR, paste0("sensB_trend_community_trend", trend_str, ".parquet")))

  # N* per window: smallest sample size where frac_detectable >= 0.80
  # NOTE: Discrete grid — N* is an upper bound on the true minimum.
  sensB_nstar <- sensB_trend_community %>%
    dplyr::group_by(siteID, scenario, window_years, window_start, window_end) %>%
    dplyr::arrange(sample_size) %>%
    dplyr::summarise(
      n_star = {
        ok <- frac_detectable >= DETECT_CUTOFF
        if (any(ok, na.rm = TRUE)) min(sample_size[ok], na.rm = TRUE) else NA_real_
      },
      .groups = "drop"
    )
  arrow::write_parquet(sensB_nstar, 
                       file.path(OUT_DIR, paste0("sensB_nstar_trend", trend_str, ".parquet")))
  message("    SensB N* complete for trend=", trend_magnitude)
  
  # Store for combined validation later
  sensB_all_by_trend[[as.character(trend_magnitude)]] <- list(
    species = sensB_trend_species,
    community = sensB_trend_community,
    nstar = sensB_nstar
  )
}

message("\n  SensB complete for all ", length(TREND_SIZES), " trend magnitude(s)")

# ==============================================================================
# 11. Sensitivity A + Slope Stability (Combined, Single Pass)
# ==============================================================================
#
# Runs baseline trend analysis (no imposed trend) with slope statistics.
# Produces both SensA outputs (detection probability) and stability outputs
# (slope mean, SD, CV across plot resamples) from a single pass through
# all windows. This halves the window processing time compared to running
# SensA and stability separately.
#
# SensA asks: "is the existing natural trend detectable at different K?"
# Stability asks: "how many plots for reliable trend estimation?"
# Both use the same slopes — SensA thresholds them, stability summarizes them.
# ==============================================================================

message("\n[6/7] Sensitivity A + slope stability (combined)...")

sensA_all <- list()

for (W in TREND_WINDOWS) {
  windows <- make_moving_windows(years_for_windows, W)
  if (is.null(windows)) next

  ex_win <- windows[[1]]
  ex_years <- paste(ex_win$years_win, collapse = ",")
  ex_threshold <- 0.20 / ex_win$time_span
  message(sprintf("  W=%d: %d windows (e.g., [%s]: %d points over %d years, threshold=%.4f/yr)",
                  W, length(windows), ex_years, ex_win$W_used, ex_win$time_span, ex_threshold))

  for (win in windows) {
    slope_threshold <- 0.20 / win$time_span
    
    base_seed <- as.numeric(site_seed(SITE_ID)) +
      20000 * W + 100 * win$window_id

    result <- run_window_trend_detection(
      pred                 = pred,
      plots_by_year        = plots_by_year,
      row_ids_by_plot_year = row_ids_by_plot_year,
      window_spec          = win,
      n_plots_grid         = N_PLOTS_GRID,
      trend_reps           = TREND_REPS,
      slope_threshold      = slope_threshold,
      impose_trend         = FALSE,
      beta_step            = 0,
      site_id              = SITE_ID,
      base_seed            = base_seed,
      return_slopes        = TRUE
    )

    if (!is.null(result)) {
      sensA_all[[length(sensA_all) + 1]] <- result
    }
  }
}

# Combined output has both detect_prob (SensA) and slope stats (stability)
sensA_combined <- dplyr::bind_rows(sensA_all) %>%
  drop_other("species")

# --- SensA outputs (detection) ---

sensA_trend_species <- sensA_combined %>%
  dplyr::select(-any_of(c("slope_mean", "slope_sd", "slope_cv")))

arrow::write_parquet(sensA_trend_species,
                     file.path(OUT_DIR, "sensA_trend_species.parquet"))
message("  SensA complete: ", nrow(sensA_trend_species), " rows")

# ==============================================================================
# 12. Community Summaries and N* for Sensitivity A
# ==============================================================================

sensA_trend_community <- sensA_trend_species %>%
  dplyr::group_by(siteID, window_years, window_start, window_end, sample_size) %>%
  dplyr::summarise(
    frac_detectable    = mean(detect_prob >= DETECT_CUTOFF, na.rm = TRUE),
    median_detect_prob = median(detect_prob, na.rm = TRUE),
    n_species          = dplyr::n(),
    .groups = "drop"
  )
arrow::write_parquet(sensA_trend_community,
                     file.path(OUT_DIR, "sensA_trend_community.parquet"))

sensA_nstar <- sensA_trend_community %>%
  dplyr::group_by(siteID, window_years, window_start, window_end) %>%
  dplyr::arrange(sample_size) %>%
  dplyr::summarise(
    n_star = {
      ok <- frac_detectable >= DETECT_CUTOFF
      if (any(ok, na.rm = TRUE)) min(sample_size[ok], na.rm = TRUE) else NA_real_
    },
    .groups = "drop"
  )
arrow::write_parquet(sensA_nstar, file.path(OUT_DIR, "sensA_nstar.parquet"))
message("  SensA N* complete")

# --- Stability outputs (slope statistics) ---

stability_species <- sensA_combined %>%
  dplyr::select(-detect_prob)

arrow::write_parquet(stability_species,
                     file.path(OUT_DIR, "stability_species.parquet"))
message("  Stability analysis complete: ", nrow(stability_species), " rows")

# Community-level stability summary: median CV across species by K
stability_community <- stability_species %>%
  dplyr::filter(is.finite(slope_cv)) %>%
  dplyr::group_by(siteID, window_years, window_start, window_end, sample_size) %>%
  dplyr::summarise(
    median_slope_cv    = median(slope_cv, na.rm = TRUE),
    mean_slope_sd      = mean(slope_sd, na.rm = TRUE),
    pct_cv_below_50    = mean(slope_cv < 0.5, na.rm = TRUE) * 100,
    pct_cv_below_25    = mean(slope_cv < 0.25, na.rm = TRUE) * 100,
    n_species          = dplyr::n(),
    .groups = "drop"
  )

arrow::write_parquet(stability_community,
                     file.path(OUT_DIR, "stability_community.parquet"))
message("  Stability community summary complete")

# ==============================================================================
# 14. Save Session Metadata and Validate Outputs
# ==============================================================================

message("\n[7/7] Validating outputs...")

session_meta <- list(
  run_config   = run_config,
  n_draws_used = n_draw,
  n_species    = n_sp,
  years        = years,
  windows_used = TREND_WINDOWS,
  trend_sizes_tested = TREND_SIZES,
  timestamp    = Sys.time(),
  session_info = sessionInfo()
)
saveRDS(session_meta, file.path(OUT_DIR, "session_metadata.rds"))

# Build expected files list: SensA files (fixed) + SensB files (per trend magnitude) + stability
expected_files <- c(
  "trend_meta.parquet",
  "baseline_trend_species.parquet",
  "sensA_trend_species.parquet", "sensA_trend_community.parquet", "sensA_nstar.parquet",
  "stability_species.parquet", "stability_community.parquet"
)

# Add SensB files for each trend magnitude
for (trend_magnitude in TREND_SIZES) {
  trend_str <- sprintf("%.2f", trend_magnitude)
  expected_files <- c(expected_files,
                     paste0("sensB_trend_species_trend", trend_str, ".parquet"),
                     paste0("sensB_trend_community_trend", trend_str, ".parquet"),
                     paste0("sensB_nstar_trend", trend_str, ".parquet"))
}

for (f in expected_files) {
  fpath <- file.path(OUT_DIR, f)
  if (!file.exists(fpath) || file.info(fpath)$size == 0) {
    stop("TrendRun failed: missing or empty output: ", f, call. = FALSE)
  }
}

message("\n========================================")
message("TrendRun Complete!")
message("========================================")
message("Site: ", SITE_ID)
message("Trend magnitudes tested: ", paste(TREND_SIZES, collapse = ", "))
message("SensB output files: ", 3 * length(TREND_SIZES), " (species, community, nstar per trend)")
message("SensA species rows: ", nrow(sensA_trend_species))
message("Stability species rows: ", nrow(stability_species))
message("Output directory: ", OUT_DIR)
message("========================================")
