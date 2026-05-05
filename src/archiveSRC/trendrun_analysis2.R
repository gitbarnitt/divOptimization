# ---- TrendRun Orchestration Script (Refactored) ----

library(dplyr)
library(arrow)
library(tibble)
library(jsonlite)


# ---- 1. Parse config/env vars ----
SITE_ID <- Sys.getenv("SITE_ID", "testsite")
INPUT_RDS <- Sys.getenv("INPUT_RDS_PATH", "input/plant_data.rds")
OUT_DIR <- file.path("outputs", "trendrun", SITE_ID)
POST_DRAWS <- as.integer(Sys.getenv("POST_DRAWS", "1000"))
EPS_COVER <- as.numeric(Sys.getenv("EPS_COVER", "0.1"))
TREND_DELTA_NET <- as.numeric(Sys.getenv("TREND_DELTA_NET", "0.20"))
TREND_WINDOWS_YEARS <- c(10, 5)
N_PLOTS_GRID <- c(30, 25, 20, 15, 10, 8, 6, 5)
TREND_REPS <- as.integer(Sys.getenv("TREND_REPS", "100"))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

 # ---- 2. Write run metadata ----
trend_cfg <- list(
  SITE_ID = SITE_ID,
  INPUT_RDS = INPUT_RDS,
  OUT_DIR = OUT_DIR,
  POST_DRAWS = POST_DRAWS,
  EPS_COVER = EPS_COVER,
  TREND_DELTA_NET = TREND_DELTA_NET,
  TREND_WINDOWS_YEARS = TREND_WINDOWS_YEARS,
  N_PLOTS_GRID = N_PLOTS_GRID,
  TREND_REPS = TREND_REPS
)
write_json(trend_cfg, file.path(OUT_DIR, "run_metadata.json"), pretty = TRUE, auto_unbox = TRUE)

# ---- 3. Load all R functions ----
r_files <- list.files("src/R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f)

# ---- 3. Load data ----
site_data <- load_neon_data(INPUT_RDS)

# ---- 4. Fit model ----
fit_result <- fit_gjam_model_test(site_data)

# ---- 5. Prepare trend_meta ----
xdata_trend <- fit_result$xdata %>%
  mutate(
    row_id = row_number(),
    year_num = as.integer(as.character(year)),
    year_centered = year_num - mean(as.integer(as.character(year)))
  )
trend_meta <- xdata_trend %>%
  select(row_id, plotID, year_num, year_centered, nlcdClass)
trend_meta$key <- paste(trend_meta$year_num, trend_meta$plotID, sep = ".")

arrow::write_parquet(trend_meta, file.path(OUT_DIR, "trend_meta.parquet"))

# ---- 6. Posterior prediction ----
pred <- manual_posterior_predict(fit_result$fit, xdata_trend)

# ---- 7. Compute annual means (mu_year) ----
years <- sort(unique(trend_meta$year_num))
n_draw <- dim(pred)[1]
n_sp   <- dim(pred)[3]
n_year <- length(years)

mu_year <- array(NA_real_, dim = c(n_draw, n_year, n_sp),
                 dimnames = list(NULL, as.character(years), dimnames(pred)[[3]]))
for (i in seq_along(years)) {
  idx <- which(trend_meta$year_num == years[i])
  mu_year[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
}

# ---- 8. Log-transform annual means (mu_year_log) ----
mu_year_log <- log(mu_year + EPS_COVER)

# ---- 9. Compute baseline slopes (per draw, per species) ----
t <- years - mean(years)
den <- sum(t^2)
mu_centered <- sweep(mu_year_log, 2, colMeans(mu_year_log, dims = 2), FUN = "-")
baseline_slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
for (d in seq_len(n_draw)) {
  baseline_slopes[d, ] <- as.numeric(crossprod(t, mu_centered[d, , ]) / den)
}

# ---- 10. Summarize baseline slopes per species ----
species_names <- dimnames(pred)[[3]]
baseline_species_summary <- tibble(
  siteID = SITE_ID,
  species = species_names,
  slope_median = apply(baseline_slopes, 2, stats::median),
  slope_lo = apply(baseline_slopes, 2, stats::quantile, probs = 0.1),
  slope_hi = apply(baseline_slopes, 2, stats::quantile, probs = 0.9),
  p_up = colMeans(baseline_slopes > 0),
  p_down = colMeans(baseline_slopes < 0),
  n_years_used = n_year
)
arrow::write_parquet(baseline_species_summary, file.path(OUT_DIR, "baseline_trend_species.parquet"))
cat("TrendRun baseline annualization, log, and slope summary complete.\n")

# ---- 11. Sensitivity B: Imposed trend detectability vs plot count ----
set.seed(123)
delta_net <- TREND_DELTA_NET
windows <- TREND_WINDOWS_YEARS
beta_delta <- log(1 + delta_net) / windows
scenarios <- tibble(
  scenario = paste0("net", round(delta_net*100), "_over_", windows, "yr"),
  window_years = windows,
  beta_delta = beta_delta
)
arrow::write_parquet(scenarios, file.path(OUT_DIR, "delta_scenarios.parquet"))

plots_by_year <- split(trend_meta$plotID, trend_meta$year_num)
plots_by_year <- lapply(plots_by_year, unique)
row_ids_by_plot_year <- split(trend_meta$row_id, trend_meta$key)

sensB_results <- list()
for (sc in seq_len(nrow(scenarios))) {
  beta <- scenarios$beta_delta[sc]
  scenario_name <- scenarios$scenario[sc]
  for (sample_size in N_PLOTS_GRID) {
    detect_mat <- matrix(NA_real_, nrow = TREND_REPS, ncol = n_sp)
    colnames(detect_mat) <- dimnames(pred)[[3]]
    for (rep in seq_len(TREND_REPS)) {
      idx_list <- lapply(years, function(yr) {
        plots <- plots_by_year[[as.character(yr)]]
        sampled_plots <- sample(plots, min(sample_size, length(plots)), replace = FALSE)
        unlist(lapply(sampled_plots, function(p) row_ids_by_plot_year[[paste(yr, p, sep = ".")]]))
      })
      mu_year_rep <- array(NA_real_, dim = c(n_draw, n_year, n_sp))
      for (i in seq_along(years)) {
        idx <- idx_list[[i]]
        if (length(idx) == 0) {
          mu_year_rep[, i, ] <- NA_real_
        } else {
          mu_year_rep[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
        }
      }
      mu_year_log_rep <- log(mu_year_rep + EPS_COVER)
      mu_year_log_imposed <- sweep(mu_year_log_rep, 2, beta * t, FUN = "+")
      mu_centered <- sweep(mu_year_log_imposed, 2, colMeans(mu_year_log_imposed, dims = 2), FUN = "-")
      slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
      for (d in seq_len(n_draw)) {
        slopes[d, ] <- as.numeric(crossprod(t, mu_centered[d, , ]) / den)
      }
      detect_mat[rep, ] <- colMeans(slopes > 0, na.rm = TRUE)
    }
    sensB_results[[paste0(scenario_name, "_n", sample_size)]] <- tibble(
      siteID = SITE_ID,
      scenario = scenario_name,
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
cat("TrendRun Sensitivity B (imposed trend detectability) complete.\n")

# ---- 12. Community-level summaries for Sensitivity B ----
sensB_trend_community <- sensB_trend_species %>%
  group_by(siteID, scenario, sample_size) %>%
  summarise(
    frac_detectable = mean(detect_prob > 0.8, na.rm = TRUE),
    median_detect_prob = median(detect_prob, na.rm = TRUE),
    .groups = "drop"
  )
arrow::write_parquet(sensB_trend_community, file.path(OUT_DIR, "sensB_trend_community.parquet"))

# ---- 13. n* calculation for Sensitivity B ----
sensB_nstar <- sensB_trend_community %>%
  group_by(siteID, scenario) %>%
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

# ---- 14. Sensitivity A: Baseline trend detectability vs plot count ----
sensA_results <- list()
for (sample_size in N_PLOTS_GRID) {
  detect_mat <- matrix(NA_real_, nrow = TREND_REPS, ncol = n_sp)
  colnames(detect_mat) <- dimnames(pred)[[3]]
  for (rep in seq_len(TREND_REPS)) {
    idx_list <- lapply(years, function(yr) {
      plots <- plots_by_year[[as.character(yr)]]
      sampled_plots <- sample(plots, min(sample_size, length(plots)), replace = FALSE)
      unlist(lapply(sampled_plots, function(p) row_ids_by_plot_year[[paste(yr, p, sep = ".")]]))
    })
    mu_year_rep <- array(NA_real_, dim = c(n_draw, n_year, n_sp))
    for (i in seq_along(years)) {
      idx <- idx_list[[i]]
      if (length(idx) == 0) {
        mu_year_rep[, i, ] <- NA_real_
      } else {
        mu_year_rep[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
      }
    }
    mu_year_log_rep <- log(mu_year_rep + EPS_COVER)
    mu_centered <- sweep(mu_year_log_rep, 2, colMeans(mu_year_log_rep, dims = 2), FUN = "-")
    slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
    for (d in seq_len(n_draw)) {
      slopes[d, ] <- as.numeric(crossprod(t, mu_centered[d, , ]) / den)
    }
    detect_mat[rep, ] <- colMeans(slopes > 0, na.rm = TRUE)
  }
  sensA_results[[paste0("n", sample_size)]] <- tibble(
    siteID = SITE_ID,
    sample_size = sample_size,
    species = colnames(detect_mat),
    detect_prob = colMeans(detect_mat, na.rm = TRUE),
    n_reps = TREND_REPS,
    n_draws = n_draw
  )
}
sensA_trend_species <- bind_rows(sensA_results)
arrow::write_parquet(sensA_trend_species, file.path(OUT_DIR, "sensA_trend_species.parquet"))

# ---- 15. Community-level summaries for Sensitivity A ----
sensA_trend_community <- sensA_trend_species %>%
  group_by(siteID, sample_size) %>%
  summarise(
    frac_detectable = mean(detect_prob > 0.8, na.rm = TRUE),
    median_detect_prob = median(detect_prob, na.rm = TRUE),
    .groups = "drop"
  )
arrow::write_parquet(sensA_trend_community, file.path(OUT_DIR, "sensA_trend_community.parquet"))

# ---- 16. n* calculation for Sensitivity A ----
sensA_nstar <- sensA_trend_community %>%
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
