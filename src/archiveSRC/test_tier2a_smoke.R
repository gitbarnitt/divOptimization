# Integration test: Tier 2A end-to-end smoke test
# Runs 1 replicate on minimal fixture: resample → pivot → fit → detect → N*

library(dplyr)
library(tidyr)

setwd("c:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src")

cat("=== Tier 2A Integration Smoke Test ===\n\n")

# Source all functions
source("R/resample_plots.R")
source("R/assert_fit_inputs.R")
source("R/fit_gjam_model_test.R")
source("R/run_tier2_replicate.R")
source("R/summarize_tier2_nstar.R")

# Create minimal realistic fixture
set.seed(42)
n_plots <- 8
n_years <- 3
n_species <- 6

cat("Creating test fixture:\n")
cat(sprintf("  Plots: %d\n", n_plots))
cat(sprintf("  Years: %d\n", n_years))
cat(sprintf("  Species: %d\n", n_species))

# Build fixture in NEON format
# CRITICAL: nlcdClass must be consistent for each (plotID, year), not per species row
plot_year_metadata <- expand.grid(
  plotID = paste0("HARV_", sprintf("%03d", 1:n_plots)),
  year = 2018:(2018 + n_years - 1),
  stringsAsFactors = FALSE
) %>%
  mutate(
    siteID = "HARV",
    nlcdClass = sample(c("deciduousForest", "evergreenForest"), n(), replace = TRUE)
  )

fixture_long <- expand.grid(
  plotID = paste0("HARV_", sprintf("%03d", 1:n_plots)),
  year = 2018:(2018 + n_years - 1),
  taxonID = paste0("Species", 1:n_species),
  stringsAsFactors = FALSE
) %>%
  left_join(plot_year_metadata, by = c("plotID", "year")) %>%
  mutate(
    # Simulate cover data with some year trends
    mean_cover = pmax(0, 20 + rnorm(n(), mean = 5 * (year - 2018), sd = 10))
  )

cat(sprintf("  Total rows: %d\n", nrow(fixture_long)))
cat(sprintf("  Unique (plotID, year): %d\n\n", nrow(plot_year_metadata)))

# --- STEP 1: Resample ---
cat("STEP 1: Resample plots (80% bootstrap)...\n")

# Prepare data for resampling
# CRITICAL: xdata and ydata must have matching rows in same order
base_keys <- fixture_long %>%
  distinct(siteID, plotID, year, nlcdClass) %>%
  arrange(plotID, year)  # Ensure consistent ordering

xdata_orig <- base_keys

ydata_orig <- fixture_long %>%
  select(siteID, plotID, year, taxonID, mean_cover) %>%
  pivot_wider(names_from = taxonID, values_from = mean_cover, values_fill = 0) %>%
  arrange(plotID, year)  # Match xdata ordering

# Verify alignment before resampling
if (nrow(xdata_orig) != nrow(ydata_orig)) {
  stop(sprintf("Test fixture construction error: xdata=%d rows, ydata=%d rows", 
               nrow(xdata_orig), nrow(ydata_orig)))
}
if (!identical(xdata_orig$plotID, ydata_orig$plotID) || 
    !identical(xdata_orig$year, ydata_orig$year)) {
  stop("Test fixture construction error: plotID/year not aligned")
}

# Resample
resampled <- resample_plots(
  xdata = xdata_orig,
  ydata = ydata_orig,
  sample_frac = 0.8,
  seed = 999,
  replacement = TRUE
)

cat(sprintf("  ✓ Resampled: %d → %d rows\n", resampled$n_rows_original, resampled$n_rows_resampled))
cat(sprintf("    Unique plots: %d/%d\n", resampled$n_plots_unique, resampled$n_plots_total))
cat(sprintf("    Duplicates: %d\n", resampled$n_duplicates))
cat(sprintf("    boot_rep_id added: %s\n\n", resampled$has_boot_rep_id))

# --- STEP 2: Fit GJAM ---
cat("STEP 2: Fit GJAM on resampled data...\n")

# Prepare site_data in expected format
site_data_resampled <- resampled$xdata_resampled %>%
  left_join(
    resampled$ydata_resampled %>% select(-siteID),
    by = c("plotID", "year", "boot_rep_id")
  ) %>%
  pivot_longer(
    cols = starts_with("Species"),
    names_to = "taxonID",
    values_to = "mean_cover"
  )

cat(sprintf("  Input rows to GJAM: %d\n", nrow(site_data_resampled)))

# Wrap fit in tryCatch to handle potential errors
fit_result <- tryCatch({
  fit_gjam_model_test(
    site_data = site_data_resampled,
    seed = 123
  )
}, error = function(e) {
  cat(sprintf("  ✗ GJAM fit FAILED: %s\n", conditionMessage(e)))
  return(NULL)
})

if (is.null(fit_result)) {
  stop("Smoke test FAILED: GJAM fit error")
}

cat(sprintf("  ✓ GJAM fit completed\n"))
cat(sprintf("    Species in model: %d\n", ncol(fit_result$ydata)))
cat(sprintf("    Rows in xdata: %d\n", nrow(fit_result$xdata)))
cat(sprintf("    Rows in ydata: %d\n\n", nrow(fit_result$ydata)))

# --- STEP 3: Detection sensitivity (simplified) ---
cat("STEP 3: Run detection sensitivity...\n")

# Create mock sensitivity results (in real pipeline this comes from run_sample_size_sensitivity_variable)
mock_sensitivity <- expand.grid(
  site = "HARV",
  year_baseline = 2018,
  year_changed = 2019,
  species = colnames(fit_result$ydata)[1:3],  # Use first 3 species
  sample_size = c(2, 4, 6, 8),
  replicate = 1
) %>%
  mutate(
    # Simulate increasing detection with sample size
    mean_detection = pmin(0.95, 0.2 + (sample_size - 2) * 0.15 + rnorm(n(), 0, 0.05))
  )

cat(sprintf("  Mock sensitivity rows: %d\n", nrow(mock_sensitivity)))

# --- STEP 4: Compute N* ---
cat("\nSTEP 4: Compute N*...\n")

n_star_result <- compute_n_star_from_sensitivity(
  sens = mock_sensitivity,
  threshold = 0.8,
  sample_size_col = "sample_size",
  detect_prob_col = "mean_detection",
  replicate_cols = c("replicate"),
  group_preference = c("site", "year_baseline", "year_changed", "species")
)

cat(sprintf("  ✓ N* computed for %d groups\n", nrow(n_star_result)))

# Display appropriate columns based on what's available
display_cols <- intersect(
  c("species", "n_star", "n_star_median", "n_star_raw", "delta_n_star", 
    "cummax_changed", "cummax_changed_freq", "never_reached", "n_reps"),
  names(n_star_result)
)

cat("\nN* Results:\n")
print(n_star_result[1:min(5, nrow(n_star_result)), display_cols])

# --- STEP 5: Verify diagnostics ---
cat("\n\nSTEP 5: Verify diagnostics...\n")

# Check for either individual or aggregated diagnostic columns
individual_cols <- c("n_star", "n_star_raw", "n_star_cummax", "delta_n_star",
                     "cummax_changed", "n_points_smoothed", "never_reached")
aggregated_cols <- c("n_star_median", "cummax_changed_freq", "mean_delta_n_star", 
                     "early_spike_freq", "fail_or_never_rate", "n_reps")

has_individual <- any(individual_cols %in% names(n_star_result))
has_aggregated <- any(aggregated_cols %in% names(n_star_result))

if (has_individual) {
  # Individual-level diagnostics (from non-aggregated output)
  if ("delta_n_star" %in% names(n_star_result)) {
    valid_deltas <- n_star_result %>%
      filter(!is.na(delta_n_star)) %>%
      mutate(
        delta_reasonable = abs(delta_n_star) <= 10  # Shouldn't differ by more than 10 plots
      )

    if (nrow(valid_deltas) > 0 && !all(valid_deltas$delta_reasonable)) {
      cat("  ⚠️  Some delta_n_star values are very large\n")
    } else if (nrow(valid_deltas) > 0) {
      cat("  ✓ delta_n_star values reasonable\n")
    }
  }
}

if (has_aggregated) {
  # Aggregated diagnostics (from multi-replicate output)
  if ("mean_delta_n_star" %in% names(n_star_result)) {
    cat(sprintf("  Mean delta_n_star: %.2f (aggregated across replicates)\n",
                mean(n_star_result$mean_delta_n_star, na.rm = TRUE)))
  }
}

# Verify expected columns are present (flexible to either format)
required_cols <- if (has_aggregated) aggregated_cols else individual_cols
present_cols <- intersect(required_cols, names(n_star_result))
missing_cols <- setdiff(required_cols, names(n_star_result))

if (length(missing_cols) > 0) {
  cat(sprintf("  ⚠️  Some expected columns missing: %s\n", 
              paste(missing_cols, collapse = ", ")))
  cat(sprintf("  ✓ But found: %s\n", paste(present_cols, collapse = ", ")))
} else {
  cat("  ✓ All diagnostic columns present\n")
}

# Check for early spike risk
if ("delta_n_star" %in% names(n_star_result)) {
  early_spikes <- n_star_result %>%
    filter(!is.na(delta_n_star), delta_n_star < -2)

  if (nrow(early_spikes) > 0) {
    cat(sprintf("  ⚠️  Early spike detected in %d/%d groups (raw N* > cummax N*)\n",
                nrow(early_spikes), nrow(n_star_result)))
  } else {
    cat("  ✓ No early spike risk detected\n")
  }
} else if ("early_spike_freq" %in% names(n_star_result)) {
  freq <- mean(n_star_result$early_spike_freq, na.rm = TRUE)
  if (freq > 0) {
    cat(sprintf("  ⚠️  Early spike frequency: %.1f%%\n", freq * 100))
  } else {
    cat("  ✓ No early spike risk detected\n")
  }
}

# --- STEP 6: Test aggregation ---
cat("\nSTEP 6: Test replicate aggregation...\n")

# Create mock replicate results
mock_replicates <- list(
  tibble::tibble(
    site = "HARV",
    replicate_id = 1,
    seed = 123,
    sample_frac = 0.8,
    n_plots_unique = 6,
    n_plots_total = 8,
    fit_status = "ok",
    fit_time_sec = 45.2,
    n_species_used = 6,
    years_used = "2018,2019,2020",
    fail_reason = NA_character_,
    n_star_median = median(n_star_result$n_star, na.rm = TRUE)
  ),
  tibble::tibble(
    site = "HARV",
    replicate_id = 2,
    seed = 124,
    sample_frac = 0.8,
    n_plots_unique = 7,
    n_plots_total = 8,
    fit_status = "ok",
    fit_time_sec = 43.8,
    n_species_used = 6,
    years_used = "2018,2019,2020",
    fail_reason = NA_character_,
    n_star_median = median(n_star_result$n_star, na.rm = TRUE) + 1
  )
)

summary_tier2 <- summarize_tier2_nstar(mock_replicates)

cat("  ✓ Aggregation completed\n")
cat("\nTier 2A Summary:\n")
print(summary_tier2[, c("site", "n_star_median", "n_star_q05", "n_star_q95", 
                         "B", "fit_fail_rate", "mean_fit_time_sec")])

# --- FINAL CHECKS ---
cat("\n\n=== FINAL VALIDATION ===\n")

checks_passed <- 0
checks_total <- 0

# Check 1: boot_rep_id added and used
checks_total <- checks_total + 1
if (resampled$has_boot_rep_id && "boot_rep_id" %in% names(resampled$xdata_resampled)) {
  cat("  ✓ boot_rep_id added for row uniqueness\n")
  checks_passed <- checks_passed + 1
} else {
  cat("  ✗ boot_rep_id missing\n")
}

# Check 2: Row alignment preserved
checks_total <- checks_total + 1
if (nrow(fit_result$xdata) == nrow(fit_result$ydata)) {
  cat(sprintf("  ✓ Row alignment preserved: %d == %d\n", 
              nrow(fit_result$xdata), nrow(fit_result$ydata)))
  checks_passed <- checks_passed + 1
} else {
  cat(sprintf("  ✗ Row mismatch: xdata=%d, ydata=%d\n",
              nrow(fit_result$xdata), nrow(fit_result$ydata)))
}

# Check 3: N* diagnostics complete
checks_total <- checks_total + 1
if (all(required_cols %in% names(n_star_result))) {
  cat("  ✓ All N* diagnostics present\n")
  checks_passed <- checks_passed + 1
} else {
  cat("  ✗ Missing N* diagnostics\n")
}

# Check 4: Aggregation produces expected structure
checks_total <- checks_total + 1
expected_summary_cols <- c("site", "n_star_median", "B", "fit_fail_rate")
if (all(expected_summary_cols %in% names(summary_tier2))) {
  cat("  ✓ Summary structure correct\n")
  checks_passed <- checks_passed + 1
} else {
  cat("  ✗ Summary structure incomplete\n")
}

# Check 5: No silent row drops
checks_total <- checks_total + 1
expected_rows <- floor(nrow(xdata_orig) * 0.8)  # approximate
if (resampled$n_rows_resampled >= expected_rows * 0.8) {  # allow some tolerance
  cat(sprintf("  ✓ Row count reasonable: %d resampled from %d\n",
              resampled$n_rows_resampled, resampled$n_rows_original))
  checks_passed <- checks_passed + 1
} else {
  cat("  ✗ Unexpectedly few rows after resampling\n")
}

cat(sprintf("\n=== SMOKE TEST: %d/%d checks passed ===\n", checks_passed, checks_total))

if (checks_passed == checks_total) {
  cat("\n=== ALL CHECKS PASSED - Ready for real data test! ===\n")
} else {
  stop(sprintf("\n=== SMOKE TEST FAILED: %d/%d checks failed ===\n", 
               checks_total - checks_passed, checks_total))
}

cat("\n")  # Final newline
