# Helper: safe integer seed
seed_i32 <- function(x) {
  m <- .Machine$integer.max
  as.integer(((x %% m) + m) %% m)
}
# Helper: deterministic int32-ish seed from string
site_seed <- function(x) {
  vals <- utf8ToInt(x)
  if (length(vals) == 0) return(1L)
  m <- .Machine$integer.max
  acc <- 0L
  for (i in seq_along(vals)) {
    acc <- as.integer((acc + (vals[i] * i) %% m) %% m)
  }
  acc + 1L
}
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
EPS_COVER <- as.numeric(Sys.getenv("EPS_COVER", "0.1")) # unused in current noisy slope version; kept for compatibility
TREND_ADD_NET <- as.numeric(Sys.getenv("TREND_ADD_NET", "0.20"))
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
fit_result <- fit_gjam_model_test(site_data)

# ---- 6. Prepare trend_meta ----
xdata_trend <- fit_result$xdata %>%
  mutate(
    row_id = row_number(),
    year_num = as.integer(as.character(year)),
    year_centered = year_num - mean(as.integer(as.character(year))) # Add year-centered (for metadata only; not used in calculations)
  )
trend_meta <- xdata_trend %>%
  select(row_id, plotID, year_num, year_centered, nlcdClass)
trend_meta$key <- paste(trend_meta$year_num, trend_meta$plotID, sep = ".")

n_years <- trend_meta$year_num
if (anyNA(n_years)) stop("Some year values could not be converted to integer.")

arrow::write_parquet(trend_meta, file.path(OUT_DIR, "trend_meta.parquet"))

# ---- 7. Posterior prediction ----
# Use observation-level predictions with residual noise (no clamp for TrendRun)
# POST_DRAWS controls number of posterior draws (subsample if set)
all_draws <- nrow(.locate_beta_matrix(fit_result$fit))
if (!is.na(POST_DRAWS) && POST_DRAWS > 0 && POST_DRAWS < all_draws) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) .Random.seed else NULL
  set.seed(seed_i32(site_seed(SITE_ID)))
  draw_idx <- sort(sample.int(all_draws, POST_DRAWS))
  if (had_seed) {
    .Random.seed <- old_seed
  } else {
    rm(.Random.seed, envir = .GlobalEnv)
  }
} else {
  draw_idx <- NULL
}
# For reproducibility, use deterministic site_seed
pred <- manual_posterior_predict_obs(
  fit_result$fit, xdata_trend,
  draws = draw_idx,
  clamp = FALSE,
  seed = site_seed(SITE_ID)
)


# Early sanity checks
stopifnot(length(dim(pred)) == 3)
stopifnot(dim(pred)[2] == nrow(xdata_trend))

# Define years, n_draw, n_sp, n_year, t
years <- sort(unique(n_years))
n_draw <- dim(pred)[1]
n_sp   <- dim(pred)[3]
n_year <- length(years)

t <- as.numeric(years) - mean(as.numeric(years))
stopifnot(all(is.finite(t)))


# Quick check: do we now have within-year variability?
yr_check <- as.integer(names(which.max(table(trend_meta$year_num))))
idx_check <- which(trend_meta$year_num == yr_check)
old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) .Random.seed else NULL
set.seed(1)
sps <- sample(dimnames(pred)[[3]], 10)
if (!is.null(old_seed)) .Random.seed <- old_seed
within_sd <- sapply(sps, function(sp) sd(pred[1, idx_check, sp], na.rm=TRUE))
cat("Within-year SD summary (draw 1, year", yr_check, "):\n")
print(summary(within_sd))
# extra sanity: unique values within year for most variable sampled species
sp_check <- sps[which.max(within_sd)]
nuniq <- dplyr::n_distinct(pred[1, idx_check, sp_check])
cat("Sanity check species:", sp_check, " | n_unique(pred within year):", nuniq, "\n")

# Check: within-year SD for many spp
set.seed(1)
sps_big <- sample(dimnames(pred)[[3]], min(200, length(dimnames(pred)[[3]])))
within_sd_big <- sapply(sps_big, function(sp) sd(pred[1, idx_check, sp], na.rm=TRUE))
cat("Within-year SD (200 spp) quantiles:\n")
print(quantile(within_sd_big, probs = c(0, .1, .25, .5, .75, .9, 1), na.rm=TRUE))

# Check: how much of within-year variation is just NLCD differences?
df_chk <- trend_meta[idx_check, c("nlcdClass")]
df_chk$mu <- pred[1, idx_check, sp_check]
print(df_chk %>% dplyr::group_by(nlcdClass) %>% dplyr::summarise(n=n(), mean_mu=mean(mu), sd_mu=sd(mu), .groups="drop"))

# Log requested vs used draws
requested_draws <- if (is.null(draw_idx)) all_draws else length(draw_idx)
cat("Requested draws:", requested_draws, " | Used draws:", dim(pred)[1], "\n")
# Save draw_idx for reproducibility/debugging
if (!is.null(draw_idx)) {
  writeLines(paste(draw_idx, collapse=","), file.path(OUT_DIR, "posterior_draw_idx.csv"))
}

# Store actual year counts for metadata
year_counts <- trend_meta %>% dplyr::count(year_num, name="n_rows") %>% dplyr::arrange(year_num)
arrow::write_parquet(year_counts, file.path(OUT_DIR, "year_counts.parquet"))


# ---- 8. Compute annual means (mu_year) from noisy predictions ----
mu_year <- array(NA_real_, dim = c(n_draw, n_year, n_sp),
                 dimnames = list(NULL, as.character(years), dimnames(pred)[[3]]))
for (i in seq_along(years)) {
  idx <- which(trend_meta$year_num == years[i])
  mu_year[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
}

# Early check after mu_year
stopifnot(dim(mu_year)[2] == length(years))
na_frac <- mean(!is.finite(mu_year))
cat("mu_year non-finite fraction:", signif(na_frac, 3), "\n")


# ---- 9. Center annual means (noisy) ----
# mu_year: [draw, year, species]
mu_mean_year <- apply(mu_year, c(1,3), mean, na.rm = TRUE)
mu_centered  <- sweep(mu_year, c(1,3), mu_mean_year, FUN = "-")

# NA-safe slope function for noisy annual means
slope_one <- function(y, t) {
  ok <- is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  sum(t[ok] * y[ok]) / sum(t[ok]^2)
}


# ---- 10. Compute baseline slopes from noisy annual means (per draw, per species) ----
baseline_slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
for (d in seq_len(n_draw)) {
  baseline_slopes[d, ] <- apply(mu_centered[d, , ], 2, slope_one, t = t)
}

# ---- 11. Summarize baseline slopes per species ----
species_names <- dimnames(pred)[[3]]
baseline_species_summary <- tibble(
  siteID = SITE_ID,
  species = species_names,
  slope_median = apply(baseline_slopes, 2, stats::median, na.rm = TRUE),
  slope_lo = apply(baseline_slopes, 2, stats::quantile, probs = 0.1, na.rm = TRUE),
  slope_hi = apply(baseline_slopes, 2, stats::quantile, probs = 0.9, na.rm = TRUE),
  p_up = colMeans(baseline_slopes > 0, na.rm = TRUE),
  p_down = colMeans(baseline_slopes < 0, na.rm = TRUE),
  n_years_used = n_year
)
arrow::write_parquet(baseline_species_summary, file.path(OUT_DIR, "baseline_trend_species.parquet"))
cat("TrendRun baseline annualization and slope summary complete.\n")


# ---- 12. Sensitivity B: Imposed trend detectability vs plot count ----
# Optionally: site-specific seed for replicate selection
# set.seed(site_seed(SITE_ID))
# Trend injection is additive on the model/latent scale:
# add_net is the net additive change across the window (years), so beta_per_year = add_net / window_years
add_net <- TREND_ADD_NET
windows <- TREND_WINDOWS_YEARS
beta_per_year <- add_net / windows
scenarios <- tibble(
  scenario = paste0("add", signif(add_net, 3), "_over_", windows, "yr"),
  window_years = windows,
  beta_per_year = beta_per_year
)
arrow::write_parquet(scenarios, file.path(OUT_DIR, "trend_add_scenarios.parquet"))

plots_by_year <- split(trend_meta$plotID, trend_meta$year_num)
plots_by_year <- lapply(plots_by_year, unique)
row_ids_by_plot_year <- split(trend_meta$row_id, trend_meta$key)

sensB_results <- list()
for (sc in seq_len(nrow(scenarios))) {
  beta <- scenarios$beta_per_year[sc]
  scenario_name <- scenarios$scenario[sc]
  for (sample_size in N_PLOTS_GRID) {
    detect_mat <- matrix(NA_real_, nrow = TREND_REPS, ncol = n_sp)
    colnames(detect_mat) <- dimnames(pred)[[3]]
    for (rep in seq_len(TREND_REPS)) {
      idx_list <- lapply(seq_along(years), function(i) {
        yr <- years[i]
        plots <- plots_by_year[[as.character(yr)]]
        set.seed(seed_i32(site_seed(SITE_ID) + 100000L*sc + 1000L*sample_size + 10L*i + rep))
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
      # NOTE: Trend is injected into annual means after residual sampling (i.e., after noise)
      # Inject trend at annual mean level (efficient, identical result)
      mu_year_imposed <- sweep(mu_year_rep, 2, beta * t, FUN = "+")
      mu_mean_year_imposed <- apply(mu_year_imposed, c(1,3), mean, na.rm = TRUE)
      mu_centered_imposed <- sweep(mu_year_imposed, c(1,3), mu_mean_year_imposed, FUN = "-")
      slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
      for (d in seq_len(n_draw)) {
        slopes[d, ] <- apply(mu_centered_imposed[d, , ], 2, slope_one, t = t)
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

# ---- 13. Community-level summaries for Sensitivity B ----
sensB_trend_community <- sensB_trend_species %>%
  group_by(siteID, scenario, sample_size) %>%
  summarise(
    frac_detectable = mean(detect_prob > 0.8, na.rm = TRUE),
    median_detect_prob = median(detect_prob, na.rm = TRUE),
    .groups = "drop"
  )
arrow::write_parquet(sensB_trend_community, file.path(OUT_DIR, "sensB_trend_community.parquet"))

# ---- 14. n* calculation for Sensitivity B ----
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

# ---- 15. Sensitivity A: Baseline trend detectability vs plot count ----
sensA_results <- list()
for (sample_size in N_PLOTS_GRID) {
  detect_mat <- matrix(NA_real_, nrow = TREND_REPS, ncol = n_sp)
  colnames(detect_mat) <- dimnames(pred)[[3]]
  for (rep in seq_len(TREND_REPS)) {
    idx_list <- lapply(seq_along(years), function(i) {
      yr <- years[i]
      plots <- plots_by_year[[as.character(yr)]]
      set.seed(seed_i32(site_seed(SITE_ID) + 1000L*sample_size + 10L*i + rep))
      sampled_plots <- sample(plots, min(sample_size, length(plots)), replace = FALSE)
      unlist(lapply(sampled_plots, function(p) row_ids_by_plot_year[[paste(yr, p, sep = ".")]]))
    })
    # Store actual year counts for metadata
    year_counts <- trend_meta %>% dplyr::count(year_num, name="n_rows") %>% dplyr::arrange(year_num)
    arrow::write_parquet(year_counts, file.path(OUT_DIR, "year_counts.parquet"))

    # Store years and t for metadata
    arrow::write_parquet(
      tibble::tibble(year = years, t = t),
      file.path(OUT_DIR, "years_and_t.parquet")
    )
    mu_year_rep <- array(NA_real_, dim = c(n_draw, n_year, n_sp))
    for (i in seq_along(years)) {
      idx <- idx_list[[i]]
      if (length(idx) == 0) {
        mu_year_rep[, i, ] <- NA_real_
      } else {
        mu_year_rep[, i, ] <- apply(pred[, idx, , drop = FALSE], c(1, 3), mean)
      }
    }
    mu_mean_year_rep <- apply(mu_year_rep, c(1,3), mean, na.rm = TRUE)
    mu_centered_rep  <- sweep(mu_year_rep, c(1,3), mu_mean_year_rep, FUN = "-")
    slopes <- matrix(NA_real_, nrow = n_draw, ncol = n_sp)
    for (d in seq_len(n_draw)) {
      slopes[d, ] <- apply(mu_centered_rep[d, , ], 2, slope_one, t = t)
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

# ---- 16. Community-level summaries for Sensitivity A ----
sensA_trend_community <- sensA_trend_species %>%
  group_by(siteID, sample_size) %>%
  summarise(
    frac_detectable = mean(detect_prob > 0.8, na.rm = TRUE),
    median_detect_prob = median(detect_prob, na.rm = TRUE),
    .groups = "drop"
  )
arrow::write_parquet(sensA_trend_community, file.path(OUT_DIR, "sensA_trend_community.parquet"))

# ---- 17. n* calculation for Sensitivity A ----
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
