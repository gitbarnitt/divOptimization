# ==============================================================================
# Test GJAM REDUCT = FALSE on Local Machine
# ==============================================================================
# This script tests the GJAM model fit with dimension reduction disabled
# Run from RStudio in the plantDivOptimizationJob/src/ directory

cat("\n=== Testing GJAM Model (REDUCT = FALSE) ===\n\n")

# ------------------------------------------------------------------------------
# 1. Set Working Directory (adjust if needed)
# ------------------------------------------------------------------------------
# Make sure you're in: plantDivOptimizationJob/src/
if (!file.exists("R/fit_gjam_model_test.R")) {
  stop("Please run this from the src/ directory")
}
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ------------------------------------------------------------------------------
# 2. Load Required Packages
# ------------------------------------------------------------------------------
cat("Loading packages...\n")
library(dplyr)
library(tidyr)
library(gjam)

# ------------------------------------------------------------------------------
# 3. Source Helper Functions
# ------------------------------------------------------------------------------
cat("Sourcing R helper functions...\n")
r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f, echo = FALSE)

# ------------------------------------------------------------------------------
# 4. Configure Test Parameters (FAST MODE)
# ------------------------------------------------------------------------------
SITE_ID <- "JERC"  # Change to any site you want to test

# Locate input data (adjust path if needed)
INPUT_RDS <- "data/plant_data.rds"  # Relative to src/

if (!file.exists(INPUT_RDS)) {
  cat("\n⚠️  Input file not found at:", INPUT_RDS, "\n")
  cat("Please adjust INPUT_RDS path or download data from GCS\n\n")
  stop("Input data not found")
}

cat(sprintf("Testing site: %s\n", SITE_ID))
cat(sprintf("Input data: %s\n", INPUT_RDS))

# ------------------------------------------------------------------------------
# 5. Load and Preprocess Data (CRITICAL: Use load_neon_data!)
# ------------------------------------------------------------------------------
cat("\nLoading and preprocessing NEON data...\n")

# Set site filter (load_neon_data reads SITE_ID env var)
Sys.setenv(SITE_ID = SITE_ID)

# This function does critical preprocessing:
#   - Quality filtering
#   - Subplot aggregation
#   - Bout averaging
#   - Creates mean_cover column
site_data <- load_neon_data(INPUT_RDS)

cat(sprintf("  Rows: %d\n", nrow(site_data)))
cat(sprintf("  Plots: %d\n", length(unique(site_data$plotID))))
cat(sprintf("  Years: %s\n", paste(sort(unique(site_data$year)), collapse = ", ")))
cat(sprintf("  Species: %d\n", length(unique(site_data$taxonID))))

# Verify required columns exist
required_cols <- c("siteID", "plotID", "year", "taxonID", "nlcdClass", "mean_cover")
missing_cols <- setdiff(required_cols, names(site_data))
if (length(missing_cols) > 0) {
  stop("Missing required columns from load_neon_data: ", paste(missing_cols, collapse = ", "))
}
cat("  ✓ All required columns present\n")

# ------------------------------------------------------------------------------
# 6. Fit GJAM Model (WATCH FOR DIMENSION REDUCTION OUTPUT!)
# ------------------------------------------------------------------------------
cat("\n" , paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("FITTING GJAM MODEL - Watch for dimension reduction messages below:\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

fit_result <- fit_gjam_model_test(site_data, seed = 123)

cat("\n" , paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("MODEL FIT COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

# ------------------------------------------------------------------------------
# 7. Verify REDUCT = FALSE (Post-Fit Check)
# ------------------------------------------------------------------------------
cat("Verifying REDUCT settings...\n\n")

# Check top-level REDUCT
if (!is.null(fit_result$fit$modelList$REDUCT)) {
  if (fit_result$fit$modelList$REDUCT == FALSE) {
    cat("  ✓ Top-level REDUCT = FALSE\n")
  } else {
    cat("  ✗ WARNING: Top-level REDUCT =", fit_result$fit$modelList$REDUCT, "\n")
  }
} else {
  cat("  ⚠️  Top-level REDUCT is NULL (may be OK)\n")
}

# Check if dimension reduction was applied
if (!is.null(fit_result$fit$modelList$reductList)) {
  cat("\n  reductList exists:\n")
  print(fit_result$fit$modelList$reductList)
} else {
  cat("  ✓ No reductList (dimension reduction not applied)\n")
}

# Check for reduced dimensions
if (!is.null(fit_result$fit$inputs$u2s)) {
  u2s_valid <- attr(fit_result$fit$inputs$u2s, "valid")
  if (is.logical(u2s_valid) && u2s_valid == FALSE) {
    cat("  ✓ u2s matrix marked as invalid (no reduction)\n")
  } else {
    cat("  ⚠️  u2s matrix may indicate dimension reduction was applied\n")
  }
}

# ------------------------------------------------------------------------------
# 8. Model Summary
# ------------------------------------------------------------------------------
cat("\n=== Model Summary ===\n")
cat(sprintf("  Site: %s\n", fit_result$site))
cat(sprintf("  Response variables: %d species\n", ncol(fit_result$ydata)))
cat(sprintf("  Observations: %d\n", nrow(fit_result$xdata)))
cat(sprintf("  Predictors: %s\n", paste(names(fit_result$xdata), collapse = ", ")))
cat(sprintf("  Iterations: %d (burnin: %d)\n", 
            fit_result$fit$modelList$ng,
            fit_result$fit$modelList$burnin))

# ------------------------------------------------------------------------------
# 9. Quick Prediction Test
# ------------------------------------------------------------------------------
cat("\n=== Testing Prediction Functions ===\n")

cat("  Testing manual_posterior_predict...\n")
pred_test <- manual_posterior_predict(
  fit = fit_result$fit,
  xnew = fit_result$xdata[1:5, ]  # Just first 5 rows
)
cat(sprintf("    ✓ Dimensions: %d draws × %d obs × %d species\n",
            dim(pred_test)[1], dim(pred_test)[2], dim(pred_test)[3]))

cat("  Testing manual_posterior_predict_obs...\n")
pred_obs_test <- manual_posterior_predict_obs(
  fit = fit_result$fit,
  xnew = fit_result$xdata[1:5, ]
)
cat(sprintf("    ✓ Dimensions: %d draws × %d obs × %d species\n",
            dim(pred_obs_test)[1], dim(pred_obs_test)[2], dim(pred_obs_test)[3]))

# ------------------------------------------------------------------------------
# Done!
# ------------------------------------------------------------------------------
cat("\n" , paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("TEST COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat("What to look for in the GJAM output above:\n")
cat("  ✓ GOOD: No mention of 'dimension reduction' or 'latent factors'\n")
cat("  ✗ BAD:  Messages like 'reducing to N factors' or 'r = X'\n\n")

cat("Next steps:\n")
cat("  1. Review the GJAM output above for dimension reduction messages\n")
cat("  2. If clean, deploy to Cloud Run for full testing\n")
cat("  3. Check Cloud Run logs for the same verification\n\n")
