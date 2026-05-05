# ==============================================================================
# Local Model Fit with Diagnostics
# ==============================================================================
# Run from RStudio in the src/ directory
# Purpose: Fit GJAM model locally and run custom diagnostics

cat("\n=== Local GJAM Model Fit & Diagnostics ===\n\n")

# ==============================================================================
# 0. Set Working Directory to src/
# ==============================================================================

# Auto-detect and set working directory based on where this script is located
if (!dir.exists("R")) {
  # Try to get the script's directory (works in RStudio when sourcing)
  script_path <- tryCatch({
    # Works when sourcing in RStudio
    dirname(sys.frame(1)$ofile)
  }, error = function(e) {
    # Fallback: try rstudioapi if available
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      dirname(rstudioapi::getActiveDocumentContext()$path)
    } else {
      NULL
    }
  })
  
  if (!is.null(script_path) && script_path != "") {
    # Script path detected - this should be src/
    setwd(script_path)
    cat(sprintf("  ✓ Changed working directory to: %s\n", getwd()))
  } else {
    # Fallback: try common locations
    if (file.exists("src/R")) {
      setwd("src")
      cat("  ✓ Changed working directory to src/\n")
    } else if (file.exists("../src/R")) {
      setwd("../src")
      cat("  ✓ Changed working directory to ../src/\n")
    } else {
      cat("\n** Cannot auto-detect working directory **\n")
      cat("Please manually set it:\n\n")
      cat('  setwd("C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src")\n\n')
      cat("Then re-run this script.\n\n")
      stop("Working directory not set", call. = FALSE)
    }
  }
}

# Verify R/ directory exists
if (!dir.exists("R")) {
  stop(sprintf("R/ directory not found in: %s\nPlease set working directory to src/", getwd()))
}

cat(sprintf("Working directory: %s\n\n", getwd()))

# ==============================================================================
# 1. Configuration (Adjust these for your testing)
# ==============================================================================

# Site to analyze
SITE_ID <- "JERC"  # Change as needed

# Input data path (adjust to where your data is)
INPUT_RDS <- "data/plant_data.rds"  # Relative to src/

# Optional: Cache the fit to avoid refitting each time
USE_FIT_CACHE <- TRUE
FIT_CACHE_PATH <- file.path("outputs", "local_cache", paste0("fit_", SITE_ID, ".rds"))

# Output directory for diagnostics
DIAGNOSTICS_DIR <- file.path("outputs", "diagnostics", SITE_ID)
dir.create(DIAGNOSTICS_DIR, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("Site: %s\n", SITE_ID))
cat(sprintf("Input: %s\n", INPUT_RDS))
cat(sprintf("Output: %s\n", DIAGNOSTICS_DIR))
cat(sprintf("Fit cache: %s\n\n", if (USE_FIT_CACHE) FIT_CACHE_PATH else "disabled"))

# ==============================================================================
# 2. Load Required Packages
# ==============================================================================

cat("Loading packages...\n")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(gjam)
  library(ggplot2)
  library(arrow)
})

# ==============================================================================
# 3. Source All Helper Functions
# ==============================================================================

cat("Sourcing R helper functions...\n")

r_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) {
  source(f, echo = FALSE)
}
cat(sprintf("  Sourced %d functions\n", length(r_files)))

# ==============================================================================
# 4. Load and Preprocess Data
# ==============================================================================

cat("\nLoading data...\n")
if (!file.exists(INPUT_RDS)) {
  stop(sprintf("Input file not found: %s", INPUT_RDS))
}

# Set SITE_ID for load_neon_data filtering
Sys.setenv(SITE_ID = SITE_ID)

# Load and preprocess (handles site filtering, quality control, aggregation)
neon_data <- load_neon_data(INPUT_RDS)

cat(sprintf("  Rows: %d\n", nrow(neon_data)))
cat(sprintf("  Plots: %d\n", length(unique(neon_data$plotID))))
cat(sprintf("  Years: %s\n", paste(sort(unique(neon_data$year)), collapse = ", ")))
cat(sprintf("  Species: %d\n", length(unique(neon_data$taxonID))))

# ==============================================================================
# 5. Fit GJAM Model (LOCAL DIAGNOSTIC VERSION with short run)
# ==============================================================================

if (USE_FIT_CACHE && file.exists(FIT_CACHE_PATH)) {
  cat("\nLoading cached model fit...\n")
  fit_result <- readRDS(FIT_CACHE_PATH)
  cat("  ✓ Loaded from cache\n")
} else {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("FITTING GJAM MODEL (LOCAL DIAGNOSTIC MODE)\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
  cat("Watch for dimension reduction messages below:\n")
  cat("  ✓ GOOD: No 'dimension reduced' messages\n")
  cat("  ✗ BAD:  'Dimension reduced from X to Y' messages\n\n")
  
  # Prepare data (same logic as fit_gjam_model_test but with custom model_list)
  set.seed(123)
  site_id <- unique(neon_data$siteID)
  
  # Pivot to wide format
  y_wide <- neon_data %>%
    tidyr::pivot_wider(
      id_cols = c("siteID", "year", "plotID", "nlcdClass"),
      names_from = taxonID,
      values_from = mean_cover,
      values_fill = 0
    )
  
  # Extract predictors
  x_data <- y_wide %>%
    select(plotID, year, nlcdClass) %>%
    mutate(across(c(year, nlcdClass, plotID), as.factor))
  
  # Extract response matrix
  y_matrix <- y_wide %>%
    select(-siteID, -plotID, -year, -nlcdClass) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  cat(sprintf("\nInitial species matrix: %d obs × %d species\n", nrow(y_matrix), ncol(y_matrix)))
  
  # AGGRESSIVE filtering for local diagnostics to avoid singularity
  # 1. Drop zero-sum species
  col_sums <- colSums(y_matrix, na.rm = TRUE)
  keep_nonzero <- col_sums > 0
  y_matrix <- y_matrix[, keep_nonzero, drop = FALSE]
  cat(sprintf("  After removing zero-sum: %d species\n", ncol(y_matrix)))
  
  # 2. Drop zero-variance species
  col_vars <- apply(y_matrix, 2, function(col) var(col, na.rm = TRUE))
  keep_var <- col_vars > 0
  y_matrix <- y_matrix[, keep_var, drop = FALSE]
  cat(sprintf("  After removing zero-variance: %d species\n", ncol(y_matrix)))
  
  # 3. Drop very rare species (present in <5% of observations) - LOCAL DIAGNOSTIC ONLY
  presence_prop <- colMeans(y_matrix > 0, na.rm = TRUE)
  keep_common <- presence_prop >= 0.05
  y_matrix <- y_matrix[, keep_common, drop = FALSE]
  cat(sprintf("  After removing rare species (<5%% presence): %d species\n", ncol(y_matrix)))
  
  # 4. Drop species with very low variance (CV < 0.1) - LOCAL DIAGNOSTIC ONLY
  col_means <- colMeans(y_matrix, na.rm = TRUE)
  col_sds <- apply(y_matrix, 2, sd, na.rm = TRUE)
  cv <- ifelse(col_means > 0, col_sds / col_means, 0)
  keep_variable <- cv >= 0.1 | col_means >= 5  # Keep if variable OR abundant
  y_matrix <- y_matrix[, keep_variable, drop = FALSE]
  cat(sprintf("  After removing low-variance species: %d species\n", ncol(y_matrix)))
  
  if (ncol(y_matrix) < 10) {
    stop("Too few species remaining after filtering. Try a site with more data.")
  }
  
  y_matrix[is.na(y_matrix)] <- 0
  colnames(y_matrix) <- trimws(colnames(y_matrix))
  y_df <- as.data.frame(y_matrix)
  
  # Build formula
  n_nlcd_types <- length(unique(x_data$nlcdClass))
  if (n_nlcd_types >= 2) {
    formula <- ~ year + nlcdClass
    cat(sprintf("Using formula with nlcdClass (%d types)\n", n_nlcd_types))
  } else {
    formula <- ~ year
    cat("[WARNING] Only 1 NLCD type - excluding nlcdClass from formula\n")
  }
  
  # LOCAL DIAGNOSTIC MODEL_LIST (short run for testing)
  model_list <- list(
    typeNames = rep("CA", ncol(y_df)),
    REDUCT    = FALSE,
    ng        = 250,    # Increased from 100 to help convergence
    burnin    = 100     # Increased from 50
  )
  
  cat("\n*** Using LOCAL DIAGNOSTIC settings ***\n")
  cat(sprintf("  ng=%d, burnin=%d (SHORT for testing)\n", 
              model_list$ng, model_list$burnin))
  cat("  REDUCT=FALSE\n")
  cat(sprintf("  Species matrix: %d obs × %d species\n\n", nrow(y_df), ncol(y_df)))
  
  # Fit model
  fit <- gjam::gjam(
    formula   = formula,
    xdata     = x_data,
    ydata     = y_df,
    modelList = model_list
  )
  
  # Ensure REDUCT stays FALSE
  fit$modelList$REDUCT <- FALSE
  
  # Manually patch chains into modelList
  fit$modelList$betaBeta <- fit$chains$bgibbs
  fit$modelList$sigmaSave <- fit$chains$sgibbs
  
  fit$xdata <- x_data
  fit$y <- y_matrix
  fit$typeNames <- model_list$typeNames
  
  # Package result
  fit_result <- list(
    fit   = fit,
    site  = site_id,
    xdata = x_data,
    ydata = y_df
  )
  
  cat("\n", paste(rep("=", 70), collapse = ""), "\n", sep = "")
  cat("MODEL FIT COMPLETE\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")
  
  # Cache for future runs
  if (USE_FIT_CACHE) {
    dir.create(dirname(FIT_CACHE_PATH), recursive = TRUE, showWarnings = FALSE)
    saveRDS(fit_result, FIT_CACHE_PATH)
    cat(sprintf("  ✓ Cached fit to: %s\n", FIT_CACHE_PATH))
  }
}

# ==============================================================================
# 6. Model Summary
# ==============================================================================

cat("\n=== Model Summary ===\n")
cat(sprintf("  Site: %s\n", fit_result$site))
cat(sprintf("  Response variables: %d species\n", ncol(fit_result$ydata)))
cat(sprintf("  Observations: %d\n", nrow(fit_result$xdata)))
cat(sprintf("  Predictors: %s\n", paste(names(fit_result$xdata), collapse = ", ")))
cat(sprintf("  Iterations: %d (burnin: %d)\n", 
            fit_result$fit$modelList$ng,
            fit_result$fit$modelList$burnin))

# Check REDUCT status
cat("\n  REDUCT Status:\n")
if (!is.null(fit_result$fit$modelList$REDUCT)) {
  cat(sprintf("    modelList$REDUCT = %s\n", fit_result$fit$modelList$REDUCT))
} else {
  cat("    modelList$REDUCT = NULL\n")
}

if (!is.null(fit_result$fit$modelList$reductList)) {
  cat("    reductList exists:\n")
  print(fit_result$fit$modelList$reductList)
} else {
  cat("    reductList = NULL\n")
}

# ==============================================================================
# 7. YOUR CUSTOM DIAGNOSTICS GO HERE
# ==============================================================================

cat("\n" , paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("RUNNING DIAGNOSTICS\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

# ---- Example Diagnostic 1: Check for dimension reduction in chains ----
cat("1. Checking chain structures...\n")

# Check beta chains
if (!is.null(fit_result$fit$chains$bgibbs)) {
  beta_dim <- dim(fit_result$fit$chains$bgibbs)
  cat(sprintf("   Beta chains: %d iterations × %d coefficients\n", beta_dim[1], beta_dim[2]))
}

# Check sigma chains
if (!is.null(fit_result$fit$chains$sgibbs)) {
  sigma_obj <- fit_result$fit$chains$sgibbs
  if (is.list(sigma_obj) && !is.null(sigma_obj[[1]])) {
    sigma_dim <- dim(sigma_obj[[1]])
    cat(sprintf("   Sigma chains: list of %d × [%d × %d] matrices\n", 
                length(sigma_obj), sigma_dim[1], sigma_dim[2]))
  } else if (is.array(sigma_obj)) {
    sigma_dim <- dim(sigma_obj)
    cat(sprintf("   Sigma chains: %s array\n", paste(sigma_dim, collapse = " × ")))
  }
}

# ---- Example Diagnostic 2: Species coverage summary ----
cat("\n2. Species coverage summary...\n")

species_counts <- neon_data %>%
  group_by(taxonID) %>%
  summarise(
    n_obs = n(),
    mean_cover = mean(mean_cover, na.rm = TRUE),
    max_cover = max(mean_cover, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_cover))

cat(sprintf("   Species with mean cover > 1%%: %d\n", 
            sum(species_counts$mean_cover > 1)))
cat(sprintf("   Species with mean cover > 5%%: %d\n", 
            sum(species_counts$mean_cover > 5)))

# Save species summary
arrow::write_parquet(
  species_counts, 
  file.path(DIAGNOSTICS_DIR, "species_coverage.parquet")
)
cat(sprintf("   ✓ Saved: %s\n", 
            file.path(DIAGNOSTICS_DIR, "species_coverage.parquet")))

# ---- Example Diagnostic 3: Plot-level summaries ----
cat("\n3. Plot-level summaries...\n")

plot_summary <- neon_data %>%
  group_by(plotID, year) %>%
  summarise(
    n_species = n_distinct(taxonID),
    total_cover = sum(mean_cover, na.rm = TRUE),
    .groups = "drop"
  )

cat(sprintf("   Plot-years analyzed: %d\n", nrow(plot_summary)))
cat(sprintf("   Species richness range: %d - %d\n", 
            min(plot_summary$n_species), 
            max(plot_summary$n_species)))

arrow::write_parquet(
  plot_summary,
  file.path(DIAGNOSTICS_DIR, "plot_summary.parquet")
)
cat(sprintf("   ✓ Saved: %s\n", 
            file.path(DIAGNOSTICS_DIR, "plot_summary.parquet")))

# ---- Add your own diagnostics below ----

# Example: Examine specific parameters
# fit_result$fit$parameters$...

# Example: Make diagnostic plots
# ggplot(...) + ...

cat("\n" , paste(rep("=", 70), collapse = ""), "\n", sep = "")
cat("DIAGNOSTICS COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n", sep = "")

cat("Results saved to:\n")
cat(sprintf("  %s\n", DIAGNOSTICS_DIR))

cat("\nModel fit available in memory as: fit_result\n")
cat("Data available in memory as: neon_data\n\n")

cat("To run more diagnostics interactively:\n")
cat("  - Use fit_result$fit for GJAM model object\n")
cat("  - Use fit_result$xdata for predictors\n")
cat("  - Use fit_result$ydata for species matrix\n")
cat("  - Use neon_data for preprocessed data\n\n")
