# Test Tier 2A implementation locally (syntax check only)
# Run this to validate new functions load without errors

library(targets)
library(dplyr)

# Set working directory
setwd("c:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src")

# Source all R functions
cat("Sourcing R functions...\n")
tar_source("R")

cat("✓ All R functions loaded successfully\n")

# Check that new functions exist
new_functions <- c(
  "resample_plots",
  "assert_fit_inputs",
  "assert_species_order",
  "assert_no_silent_drop",
  "run_tier2_replicate",
  "summarize_tier2_nstar",
  "compute_n_star_from_sensitivity"
)

for (fn in new_functions) {
  if (!exists(fn)) {
    stop(sprintf("Function not found: %s", fn))
  }
  cat(sprintf("  ✓ %s\n", fn))
}

cat("\n✓ All Tier 2A functions loaded successfully!\n")
cat("\nTIER_MODE settings:\n")
cat(sprintf("  TIER_MODE = %s (default: tier1)\n", Sys.getenv("TIER_MODE", "tier1")))
cat(sprintf("  TIER2_REPS = %s (default: 25)\n", Sys.getenv("TIER2_REPS", "25")))
cat(sprintf("  TIER2_SAMPLE_FRAC = %s (default: 0.8)\n", Sys.getenv("TIER2_SAMPLE_FRAC", "0.8")))

cat("\nTo run Tier 2A, set environment variables:\n")
cat("  Sys.setenv(TIER_MODE = 'tier2a')\n")
cat("  tar_make()\n")
