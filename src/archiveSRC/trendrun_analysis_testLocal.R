# Helper: safe integer seed
seed_i32 <- function(x) {
  m <- as.numeric(.Machine$integer.max)
  x <- as.numeric(x)
  if (!is.finite(x)) return(1L)
  as.integer(((x %% m) + m) %% m) + 1L
}

# Helper: deterministic int32-ish seed from string
site_seed <- function(x) {
  site_seed <- function(x) {
    vals <- utf8ToInt(x)
    if (length(vals) == 0) return(1L)
    m <- as.numeric(.Machine$integer.max)
    acc <- 0.0
    for (i in seq_along(vals)) {
      # all double math; fold each step to prevent growth/overflow
      acc <- (acc + (as.numeric(vals[i]) * i)) %% m
    }
    seed_i32(acc)
  }

# ---- TrendRun Orchestration Script (Refactored) ----
library(dplyr)
library(arrow)
library(tibble)
library(jsonlite)

# ---- 1. Parse config/env vars ----
SITE_ID <- Sys.getenv("SITE_ID", "testsite")
INPUT_RDS <- Sys.getenv("INPUT_RDS_PATH", "C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/data/plant_data.rds")
#INPUT_RDS <- readRDS("C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/data/plant_data.rds")
OUTPUT_BASE_DIR <- Sys.getenv("OUTPUT_BASE_DIR", "")
if (nzchar(OUTPUT_BASE_DIR)) {
  OUT_DIR <- file.path(OUTPUT_BASE_DIR, "outputs", "trendrun", SITE_ID)
} else {
  OUT_DIR <- file.path("outputs", "trendrun", SITE_ID)
}
POST_DRAWS <- as.integer(Sys.getenv("POST_DRAWS", "1000"))
EPS_COVER <- as.numeric(Sys.getenv("EPS_COVER", "0.1")) # kept for compatibility
TREND_ADD_NET <- as.numeric(Sys.getenv("TREND_ADD_NET", "0.20"))

# Trailing windows (years) for TrendRun
TREND_WINDOWS_YEARS <- c(10, 5)

N_PLOTS_GRID <- c(30, 25, 20, 15, 10, 8, 6, 5)
TREND_REPS <- as.integer(Sys.getenv("TREND_REPS", "100"))

SITE_ID     <- "JERC"   # change as needed
POST_DRAWS  <- 200      # fast
TREND_REPS  <- 10       # fast
TREND_ADD_NET <- 0.20   # net change over window
N_PLOTS_GRID <- c(30, 15, 5)

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 2. Write run metadata ----
trend_cfg <- list(
  SITE_ID = SITE_ID,
  INPUT_RDS = INPUT_RDS,
  OUT_DIR = OUT_DIR,
  POST_DRAWS = POST_DRAWS,
  EPS_COVER = EPS_COVER,
  TREND_ADD_NET = TREND_ADD_NET,
  TREND_WINDOWS_YEARS = TREND_WINDOWS_YEARS,
  N_PLOTS_GRID = N_PLOTS_GRID,
  TREND_REPS = TREND_REPS
)
write_json(trend_cfg, file.path(OUT_DIR, "run_metadata.json"), pretty = TRUE, auto_unbox = TRUE)

# ---- 3. Load all R functions ----
r_files <- list.files("src/R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f)

# ---- 4. Load data ----
site_data <- load_neon_data(INPUT_RDS)

# ---- 5. Fit model ----
# ---- 5. Fit model (optionally from cache) ----
USE_FIT_CACHE <- Sys.getenv("USE_FIT_CACHE", "0") == "1"
FIT_RESULT_RDS <- Sys.getenv("FIT_RESULT_RDS", "")

if (USE_FIT_CACHE && nzchar(FIT_RESULT_RDS) && file.exists(FIT_RESULT_RDS)) {
  cat("Loading fit_result from cache:\n  ", FIT_RESULT_RDS, "\n", sep = "")
  fit_result <- readRDS(FIT_RESULT_RDS)
} else {
  cat("Fitting model (cache miss or disabled)...\n")
  fit_result <- fit_gjam_model_test(site_data)
  
  # If a path is provided, write it for reuse
  if (nzchar(FIT_RESULT_RDS)) {
    dir.create(dirname(FIT_RESULT_RDS), recursive = TRUE, showWarnings = FALSE)
    saveRDS(fit_result, FIT_RESULT_RDS)
    cat("Saved fit_result to cache:\n  ", FIT_RESULT_RDS, "\n", sep = "")
  }
}

# ---- 6. Prepare trend_meta ----
xdata_trend <- fit_result$xdata %>%
  mutate(
    row_id = row_number(),
    year_num = as.integer(as.character(year)),
    year_centered = year_num - mean(as.integer(as.character(year)))
  )

trend_meta <- xdata_trend %>%
  select(row_id, plotID, year_num, year_centered, nlcdClass)

trend_meta$key <- paste(trend_meta$year_num, trend_meta$plotID, sep = ".")

n_years <- trend_meta$year_num
if (anyNA(n_years)) stop("Some year values could not be converted to integer.")

arrow::write_parquet(trend_meta, file.path(OUT_DIR, "trend_meta.parquet"))

# Store actual year counts for metadata ONCE (was previously repeated inside loops in some versions)
year_counts <- trend_meta %>% dplyr::count(year_num, name = "n_rows") %>% dplyr::arrange(year_num)
arrow::write_parquet(year_counts, file.path(OUT_DIR, "year_counts.parquet"))

# ---- 7. Posterior prediction ----
# Use observation-level predictions with residual noise (no clamp for TrendRun)
all_draws <- nrow(.locate_beta_matrix(fit_result$fit))

if (!is.na(POST_DRAWS) && POST_DRAWS > 0 && POST_DRAWS < all_draws) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) .Random.seed else NULL
  
  set.seed(site_seed(SITE_ID))
  draw_idx <- sort(sample.int(all_draws, POST_DRAWS))
  
  if (had_seed) {
    .Random.seed <- old_seed
  } else {
    rm(.Random.seed, envir = .GlobalEnv)
  }
} else {
  draw_idx <- NULL
}

pred <- manual_posterior_predict_obs(
  fit_result$fit, xdata_trend,
  draws = draw_idx,
  clamp = FALSE,
  seed = site_seed(SITE_ID)
)

# Early sanity checks
stopifnot(length(dim(pred)) == 3)
stopifnot(dim(pred)[2] == nrow(xdata_trend))

# Define years, n_draw, n_sp, n_year
years <- sort(unique(n_years))
n_draw <- dim(pred)[1]
n_sp   <- dim(pred)[3]
n_year <- length(years)

# Log requested vs used draws
requested_draws <- if (is.null(draw_idx)) all_draws else length(draw_idx)
cat("Requested draws:", requested_draws, " | Used draws:", dim(pred)[1], "\n")
if (!is.null(draw_idx)) {
  writeLines(paste(draw_idx, collapse=","), file.path(OUT_DIR, "posterior_draw_idx.csv"))
}

# ---- OPTIONAL DIAGNOSTICS ----
# Kept plot-to-plot variability diagnostics, but removed NLCD-specific summaries per your request.
# You can turn this block on/off without affecting pipeline outputs.
if (isTRUE(as.logical(Sys.getenv("TREND_DIAGNOSTICS", "TRUE")))) {
  yr_check <- as.integer(names(which.max(table(trend_meta$year_num))))
  idx_check <- which(trend_meta$year_num == yr_check)
  
  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
  set.seed(1)
  sps <- sample(dimnames(pred)[[3]], min(10, length(dimnames(pred)[[3]])))
  if (!is.null(old_seed)) .Random.seed <- old_seed
  
  within_sd <- sapply(sps, function(sp) sd(pred[1, idx_check, sp], na.rm = TRUE))
  cat("Within-year SD summary (draw 1, year", yr_check, "):\n")
  print(summary(within_sd))
  
  sp_check <- sps[which.max(within_sd)]
  nuniq <- dplyr::n_distinct(pred[1, idx_check, sp_check])
  cat("Sanity check species:", sp_check, " | n_unique(pred within year):", nuniq, "\n")
  
  set.seed(1)
  sps_big <- sample(dimnames(pred)[[3]], min(200, length(dimnames(pred)[[3]])))
  within_sd_big <- sapply(sps_big, function(sp) sd(pred[1, idx_check, sp], na.rm = TRUE))
  cat("Within-year SD (200 spp) quantiles:\n")
  print(quantile(within_sd_big, probs = c(0, .1, .25, .5, .75, .9, 1), na.rm = TRUE))
}

# ---- 8. Helper: build trailing window years + time index ----
# We treat "trend" as a net additive change over the window:
# beta_step = TREND_ADD_NET / (W - 1) so that first->last year shift equals TREND_ADD_NET.
make_trailing_window <- function(years_all, W) {
  if (length(years_all) < 3) return(NULL)
  if (length(years_all) < W) {
    yrs <- years_all
  } else {
    yrs <- tail(years_all, W)
  }
  if (length(yrs) < 3) return(NULL)
  
  time_raw <- 0:(length(yrs) - 1)
  t <- time_raw - mean(time_raw)
  
  list(
    years_win = yrs,
    time_raw = time_raw,
    t = t,
    W_used = length(yrs)
  )
}

# Store years for metadata ONCE (full record)
arrow::write_parquet(
  tibble::tibble(year = years),
  file.path(OUT_DIR, "years_available.parquet")
)

# ---- 9. NA-safe slope function for annual means ----
slope_one <- function(y, t) {
  ok <- is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  sum(t[ok] * y[ok]) / sum(t[ok]^2)
}

# ---- 10. Baseline slopes (FULL RECORD, kept as-is for continuity) ----
# Compute annual means across all plots within each year, then slope across all years.
mu_year_full <- array(NA_real_, dim = c(n_draw, n_year, n_sp),
                      dimnames = list(NULL, as.character(years), dimnames(pred)[[3]]))
for (i in seq_along(years)) {
  idx <- which(trend_meta$year_num == years[i])
  mu_year_full[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
}

na_frac <- mean(!is.finite(mu_year_full))
cat("mu_year_full non-finite fraction:", signif(na_frac, 3), "\n")

# Center across years (per draw, per species)
mu_mean_year_full <- apply(mu_year_full, c(1, 3), mean, na.rm = TRUE)
mu_centered_full  <- sweep(mu_year_full, c(1, 3), mu_mean_year_full, FUN = "-")

# Time index for full record: use year numeric centered (original behavior)
t_full <- as.numeric(years) - mean(as.numeric(years))
stopifnot(all(is.finite(t_full)))

baseline_slopes_full <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
for (d in seq_len(n_draw)) {
  baseline_slopes_full[d, ] <- apply(mu_centered_full[d, , ], 2, slope_one, t = t_full)
}

species_names <- dimnames(pred)[[3]]
baseline_species_summary <- tibble(
  siteID = SITE_ID,
  species = species_names,
  slope_median = apply(baseline_slopes_full, 2, stats::median, na.rm = TRUE),
  slope_lo = apply(baseline_slopes_full, 2, stats::quantile, probs = 0.1, na.rm = TRUE),
  slope_hi = apply(baseline_slopes_full, 2, stats::quantile, probs = 0.9, na.rm = TRUE),
  p_up = colMeans(baseline_slopes_full > 0, na.rm = TRUE),
  p_down = colMeans(baseline_slopes_full < 0, na.rm = TRUE),
  n_years_used = n_year
)
arrow::write_parquet(baseline_species_summary, file.path(OUT_DIR, "baseline_trend_species.parquet"))
cat("TrendRun baseline (full record) annualization and slope summary complete.\n")

# ---- 11. Precompute mappings for resampling ----
plots_by_year <- split(trend_meta$plotID, trend_meta$year_num)
plots_by_year <- lapply(plots_by_year, unique)
row_ids_by_plot_year <- split(trend_meta$row_id, trend_meta$key)

# ---- 12. Sensitivity B: Imposed trend detectability vs plot count (TRAILING WINDOWS) ----
# Estimand: trailing-window NET change = TREND_ADD_NET over window W (W years -> W-1 steps).
add_net <- TREND_ADD_NET

scenarios <- tibble(
  scenario = paste0("net", signif(add_net, 3), "_over_", TREND_WINDOWS_YEARS, "yr"),
  window_years = TREND_WINDOWS_YEARS
)
arrow::write_parquet(scenarios, file.path(OUT_DIR, "trend_add_scenarios.parquet"))

sensB_results <- list()

for (sc in seq_len(nrow(scenarios))) {
  W <- scenarios$window_years[sc]
  wspec <- make_trailing_window(years, W)
  if (is.null(wspec)) next
  
  years_win <- wspec$years_win
  t_win <- wspec$t
  time_raw <- wspec$time_raw
  W_used <- wspec$W_used
  
  # Store years and t for metadata (per window) ONCE
  arrow::write_parquet(
    tibble::tibble(window_years = W, year = years_win, time_raw = time_raw, t = t_win),
    file.path(OUT_DIR, paste0("years_and_t_window_", W, "yr.parquet"))
  )
  
  # Net change over window => per-step increment over (W_used-1) steps
  beta_step <- add_net / (W_used - 1)
  
  scenario_name <- scenarios$scenario[sc]
  
  for (sample_size in N_PLOTS_GRID) {
    detect_mat <- matrix(NA_real_, nrow = TREND_REPS, ncol = n_sp)
    colnames(detect_mat) <- dimnames(pred)[[3]]
    
    for (rep in seq_len(TREND_REPS)) {
      
      # sample plots separately within each year of the trailing window
      idx_list <- lapply(seq_along(years_win), function(i) {
        yr <- years_win[i]
        plots <- plots_by_year[[as.character(yr)]]
        if (is.null(plots) || length(plots) == 0) return(integer(0))
        
        set.seed(seed_i32(
          as.numeric(site_seed(SITE_ID)) +
            100000 * sc +
            1000 * sample_size +
            10 * i +
            rep
        ))
        sampled_plots <- sample(plots, min(sample_size, length(plots)), replace = FALSE)
        
        unlist(lapply(sampled_plots, function(p) {
          row_ids_by_plot_year[[paste(yr, p, sep = ".")]]
        }), use.names = FALSE)
      })
      
      # annual means for this replicate, restricted to trailing window
      mu_year_rep <- array(NA_real_, dim = c(n_draw, W_used, n_sp),
                           dimnames = list(NULL, as.character(years_win), dimnames(pred)[[3]]))
      for (i in seq_along(years_win)) {
        idx <- idx_list[[i]]
        if (length(idx) == 0) {
          mu_year_rep[, i, ] <- NA_real_
        } else {
          mu_year_rep[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
        }
      }
      
      # Inject trend at annual mean level (efficient, stable)
      # Net change over window is beta_step*(W_used-1), implemented via time_raw
      mu_year_imposed <- sweep(mu_year_rep, 2, beta_step * time_raw, FUN = "+")
      
      # Center (per draw, per species) within the window
      mu_mean_year_imposed <- apply(mu_year_imposed, c(1, 3), mean, na.rm = TRUE)
      mu_centered_imposed  <- sweep(mu_year_imposed, c(1, 3), mu_mean_year_imposed, FUN = "-")
      
      slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
      for (d in seq_len(n_draw)) {
        slopes[d, ] <- apply(mu_centered_imposed[d, , ], 2, slope_one, t = t_win)
      }
      
      # “Detectability” defined as posterior probability slope > 0
      detect_mat[rep, ] <- colMeans(slopes > 0, na.rm = TRUE)
    }
    
    sensB_results[[paste0("W", W, "_", scenario_name, "_n", sample_size)]] <- tibble(
      siteID = SITE_ID,
      scenario = scenario_name,
      window_years = W,
      sample_size = sample_size,
      species = colnames(detect_mat),
      detect_prob = colMeans(detect_mat, na.rm = TRUE),
      n_reps = TREND_REPS,
      n_draws = n_draw
    )
  }
}

sensB_trend_species <- bind_rows(sensB_results)
arrow::write_parquet(sensB_trend_species, file.path(OUT_DIR, "sensB_trend_species.parquet"))
cat("TrendRun Sensitivity B (imposed trailing-window trend detectability) complete.\n")

# ---- 13. Community-level summaries for Sensitivity B ----
sensB_trend_community <- sensB_trend_species %>%
  group_by(siteID, scenario, window_years, sample_size) %>%
  summarise(
    frac_detectable = mean(detect_prob > 0.8, na.rm = TRUE),
    median_detect_prob = median(detect_prob, na.rm = TRUE),
    .groups = "drop"
  )
arrow::write_parquet(sensB_trend_community, file.path(OUT_DIR, "sensB_trend_community.parquet"))

# ---- 14. n* calculation for Sensitivity B ----
sensB_nstar <- sensB_trend_community %>%
  group_by(siteID, scenario, window_years) %>%
  arrange(sample_size) %>%
  summarise(
    n_star = {
      ok <- frac_detectable >= 0.8
      if (any(ok, na.rm = TRUE)) min(sample_size[ok], na.rm = TRUE) else NA_real_
    },
    .groups = "drop"
  )
arrow::write_parquet(sensB_nstar, file.path(OUT_DIR, "sensB_nstar.parquet"))
cat("TrendRun Sensitivity B n* complete.\n")

# ---- 15. Sensitivity A: Baseline trend detectability vs plot count (TRAILING WINDOWS) ----
# Baseline here = do we infer slope > 0 from the *existing* fitted posterior (no imposed trend),
# using replicate plot subsamples and restricting to trailing windows.
sensA_results <- list()

for (W in TREND_WINDOWS_YEARS) {
  wspec <- make_trailing_window(years, W)
  if (is.null(wspec)) next
  
  years_win <- wspec$years_win
  t_win <- wspec$t
  W_used <- wspec$W_used
  
  # Store years and t for metadata (per window) ONCE
  arrow::write_parquet(
    tibble::tibble(window_years = W, year = years_win, time_raw = wspec$time_raw, t = t_win),
    file.path(OUT_DIR, paste0("years_and_t_window_", W, "yr.parquet"))
  )
  
  for (sample_size in N_PLOTS_GRID) {
    detect_mat <- matrix(NA_real_, nrow = TREND_REPS, ncol = n_sp)
    colnames(detect_mat) <- dimnames(pred)[[3]]
    
    for (rep in seq_len(TREND_REPS)) {
      idx_list <- lapply(seq_along(years_win), function(i) {
        yr <- years_win[i]
        plots <- plots_by_year[[as.character(yr)]]
        if (is.null(plots) || length(plots) == 0) return(integer(0))
        
        set.seed(seed_i32(
          as.numeric(site_seed(SITE_ID)) +
            100000 * sc +
            1000 * sample_size +
            10 * i +
            rep
        ))
        sampled_plots <- sample(plots, min(sample_size, length(plots)), replace = FALSE)
        
        unlist(lapply(sampled_plots, function(p) {
          row_ids_by_plot_year[[paste(yr, p, sep = ".")]]
        }), use.names = FALSE)
      })
      
      mu_year_rep <- array(NA_real_, dim = c(n_draw, W_used, n_sp),
                           dimnames = list(NULL, as.character(years_win), dimnames(pred)[[3]]))
      for (i in seq_along(years_win)) {
        idx <- idx_list[[i]]
        if (length(idx) == 0) {
          mu_year_rep[, i, ] <- NA_real_
        } else {
          mu_year_rep[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
        }
      }
      
      mu_mean_year_rep <- apply(mu_year_rep, c(1, 3), mean, na.rm = TRUE)
      mu_centered_rep  <- sweep(mu_year_rep, c(1, 3), mu_mean_year_rep, FUN = "-")
      
      slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
      for (d in seq_len(n_draw)) {
        slopes[d, ] <- apply(mu_centered_rep[d, , ], 2, slope_one, t = t_win)
      }
      
      detect_mat[rep, ] <- colMeans(slopes > 0, na.rm = TRUE)
    }
    
    sensA_results[[paste0("W", W, "_n", sample_size)]] <- tibble(
      siteID = SITE_ID,
      window_years = W,
      sample_size = sample_size,
      species = colnames(detect_mat),
      detect_prob = colMeans(detect_mat, na.rm = TRUE),
      n_reps = TREND_REPS,
      n_draws = n_draw
    )
  }
}

sensA_trend_species <- bind_rows(sensA_results)
arrow::write_parquet(sensA_trend_species, file.path(OUT_DIR, "sensA_trend_species.parquet"))

# ---- 16. Community-level summaries for Sensitivity A ----
sensA_trend_community <- sensA_trend_species %>%
  group_by(siteID, window_years, sample_size) %>%
  summarise(
    frac_detectable = mean(detect_prob > 0.8, na.rm = TRUE),
    median_detect_prob = median(detect_prob, na.rm = TRUE),
    .groups = "drop"
  )
arrow::write_parquet(sensA_trend_community, file.path(OUT_DIR, "sensA_trend_community.parquet"))

# ---- 17. n* calculation for Sensitivity A ----
sensA_nstar <- sensA_trend_community %>%
  group_by(siteID, window_years) %>%
  arrange(sample_size) %>%
  summarise(
    n_star = {
      ok <- frac_detectable >= 0.8
      if (any(ok, na.rm = TRUE)) min(sample_size[ok], na.rm = TRUE) else NA_real_
    },
    .groups = "drop"
  )
arrow::write_parquet(sensA_nstar, file.path(OUT_DIR, "sensA_nstar.parquet"))
cat("TrendRun Sensitivity A complete.\n")
