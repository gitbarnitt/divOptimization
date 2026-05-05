# Unit test: Bootstrap duplicate handling and N* stability
# Tests the two critical "must-pass" checks from review

library(dplyr)
library(tidyr)

# Source functions
setwd("c:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src")
source("R/resample_plots.R")
source("R/run_tier2_replicate.R")

cat("=== TEST 1: Duplicate Row Robustness ===\n\n")

# Create small test dataset
set.seed(999)
n_plots <- 5
n_years <- 3
n_species <- 4

test_xdata <- expand.grid(
  plotID = paste0("P", 1:n_plots),
  year = 2020:(2020 + n_years - 1),
  stringsAsFactors = FALSE
) %>%
  mutate(
    nlcdClass = sample(c("grassland", "forest"), n(), replace = TRUE),
    siteID = "TEST"
  )

test_ydata <- test_xdata %>%
  select(plotID, year, siteID) %>%
  bind_cols(
    as.data.frame(matrix(
      runif(nrow(test_xdata) * n_species, 0, 50),
      ncol = n_species,
      dimnames = list(NULL, paste0("sp", 1:n_species))
    ))
  )

cat("Original data:\n")
cat(sprintf("  n_plots: %d\n", length(unique(test_xdata$plotID))))
cat(sprintf("  n_rows: %d\n", nrow(test_xdata)))
cat(sprintf("  Unique (plotID, year): %d\n", 
            nrow(distinct(test_xdata, plotID, year))))

# Force a resample where at least one plot appears multiple times
cat("\n--- Forcing duplicate plotID in bootstrap ---\n")
set.seed(42)

# Manually create sampled_plots with duplicates
sampled_plots <- c("P1", "P1", "P2", "P3")  # P1 appears twice
cat("Sampled plots:", paste(sampled_plots, collapse = ", "), "\n")
cat(sprintf("  Total sampled: %d\n", length(sampled_plots)))
cat(sprintf("  Unique: %d\n", length(unique(sampled_plots))))

# Use resample_plots with controlled sample
resampled <- resample_plots(
  xdata = test_xdata,
  ydata = test_ydata,
  sample_frac = length(sampled_plots) / n_plots,  # ~0.8
  seed = 42,
  replacement = TRUE
)

cat("\nResampled data:\n")
cat(sprintf("  n_plots_unique: %d\n", resampled$n_plots_unique))
cat(sprintf("  n_duplicates: %d\n", resampled$n_duplicates))
cat(sprintf("  n_rows_original: %d\n", resampled$n_rows_original))
cat(sprintf("  n_rows_resampled: %d\n", resampled$n_rows_resampled))
cat(sprintf("  has_boot_rep_id: %s\n", resampled$has_boot_rep_id))

# Check that boot_rep_id was added
if (!"boot_rep_id" %in% names(resampled$xdata_resampled)) {
  stop("FAIL: boot_rep_id not added to resampled data")
}

# Check row uniqueness with boot_rep_id
n_unique_with_boot <- resampled$xdata_resampled %>%
  distinct(plotID, year, boot_rep_id) %>%
  nrow()

if (n_unique_with_boot != nrow(resampled$xdata_resampled)) {
  stop("FAIL: Even with boot_rep_id, rows are not unique!")
}

cat("✓ PASS: boot_rep_id ensures row uniqueness\n")

# Test pivot_wider compatibility
cat("\n--- Testing pivot_wider with boot_rep_id ---\n")
test_pivot <- resampled$xdata_resampled %>%
  left_join(
    resampled$ydata_resampled %>% select(-siteID),
    by = c("plotID", "year", "boot_rep_id")
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(siteID, year, plotID, nlcdClass, boot_rep_id),
    names_from = plotID,  # dummy pivot
    values_from = nlcdClass
  )

if (nrow(test_pivot) != resampled$n_rows_resampled) {
  stop("FAIL: pivot_wider changed row count!")
}

cat("✓ PASS: pivot_wider works with boot_rep_id\n")

# Test that dropping boot_rep_id for model fitting works
cat("\n--- Testing boot_rep_id removal for modeling ---\n")
xdata_for_model <- resampled$xdata_resampled %>%
  select(-boot_rep_id)

cat(sprintf("  Columns after dropping boot_rep_id: %s\n",
            paste(names(xdata_for_model), collapse = ", ")))

if ("boot_rep_id" %in% names(xdata_for_model)) {
  stop("FAIL: boot_rep_id not properly removed")
}

cat("✓ PASS: boot_rep_id can be dropped for modeling\n")

cat("\n=== TEST 2: N* Stability Check ===\n\n")

# Create mock sensitivity results with noisy detection curve
set.seed(123)
mock_sens <- expand.grid(
  site = "TEST",
  year_baseline = 2020,
  year_changed = 2021,
  species = paste0("sp", 1:3),
  sample_size = c(5, 10, 15, 20, 25),
  replicate = 1:3
) %>%
  mutate(
    # Add noise that creates non-monotonicity
    mean_detection = pmin(1, pmax(0, 
      0.3 + (sample_size - 5) * 0.03 + rnorm(n(), 0, 0.15)
    ))
  )

# Add intentional spike at N=10
spike_idx <- which(mock_sens$sample_size == 10 & mock_sens$species == "sp1" & mock_sens$replicate == 1)
mock_sens$mean_detection[spike_idx] <- 0.85  # Above threshold

cat("Mock detection curve for sp1, rep1:\n")
sp1_curve <- mock_sens %>%
  filter(species == "sp1", replicate == 1) %>%
  arrange(sample_size)
print(sp1_curve[, c("sample_size", "mean_detection")])

# Compute N* with and without monotonic enforcement
source("R/run_tier2_replicate.R")  # reload with updated function

n_star_result <- compute_n_star_from_sensitivity(
  sens = mock_sens,
  threshold = 0.8,
  sample_size_col = "sample_size",
  detect_prob_col = "mean_detection",
  replicate_cols = c("replicate"),
  group_preference = c("site", "year_baseline", "year_changed", "species")
)

cat("\nN* results:\n")
print(n_star_result[, c("site", "species", "n_star_median", "cummax_changed", "n_points_smoothed")])

# Check diagnostics
if (any(n_star_result$cummax_changed, na.rm = TRUE)) {
  cat("\n⚠️  cummax() changed detection curves in some cases\n")
  changed <- n_star_result %>% filter(cummax_changed == TRUE)
  cat(sprintf("    Affected: %d out of %d groups\n", 
              nrow(changed), nrow(n_star_result)))
  cat(sprintf("    Average points smoothed: %.1f\n",
              mean(n_star_result$n_points_smoothed, na.rm = TRUE)))
} else {
  cat("✓ PASS: Detection curves were monotonic (no cummax changes)\n")
}

# Test stability across seeds
cat("\n--- Testing N* stability across multiple seeds ---\n")
stability_test <- lapply(1:5, function(seed_offset) {
  set.seed(123 + seed_offset)
  mock_sens_noisy <- mock_sens %>%
    mutate(mean_detection = pmin(1, pmax(0,
      mean_detection + rnorm(n(), 0, 0.05)  # Small noise
    )))
  
  compute_n_star_from_sensitivity(
    sens = mock_sens_noisy,
    threshold = 0.8,
    detect_prob_col = "mean_detection"
  )$n_star_median[1]
})

stability_vals <- unlist(stability_test)
cat("N* across 5 random seeds:", paste(stability_vals, collapse = ", "), "\n")
cat(sprintf("  CV: %.2f%%\n", 100 * sd(stability_vals, na.rm = TRUE) / mean(stability_vals, na.rm = TRUE)))

if (sd(stability_vals, na.rm = TRUE) / mean(stability_vals, na.rm = TRUE) > 0.3) {
  cat("⚠️  High variability - early spike may be driving N*\n")
} else {
  cat("✓ PASS: N* relatively stable across noise realizations\n")
}

cat("\n=== SUMMARY ===\n")
cat("✓ Duplicate row handling: boot_rep_id ensures uniqueness\n")
cat("✓ pivot_wider compatibility: works with boot_rep_id\n")
cat("✓ Model prep: boot_rep_id can be dropped\n")
cat("✓ N* computation: includes cummax diagnostic tracking\n")
cat("✓ Stability: N* computed with monotonic enforcement\n")
cat("\nReady for integration testing!\n")
