# test_pipeline.R
# Quick test script to check if the pipeline works with the fixes

cat("\n=== Testing Plant Diversity Optimization Pipeline ===\n\n")

# Check if we're in the right directory
if (!file.exists("_targets.R")) {
  stop("Please run this script from the src/ directory")
}

cat("1. Loading targets package...\n")
library(targets)

cat("2. Checking pipeline structure...\n")
tar_manifest()

cat("\n3. Visualizing pipeline...\n")
if (requireNamespace("visNetwork", quietly = TRUE)) {
  tar_visnetwork()
} else {
  cat("  (Install visNetwork to see pipeline graph)\n")
}

cat("\n4. Checking for outdated targets...\n")
outdated <- tar_outdated()
if (length(outdated) > 0) {
  cat(sprintf("  %d targets need updating:\n", length(outdated)))
  cat(paste0("    - ", outdated, "\n"))
} else {
  cat("  All targets up to date!\n")
}

cat("\n5. Validating meta data...\n")
meta <- tar_meta()
errors <- meta[!is.na(meta$error), c("name", "error")]
if (nrow(errors) > 0) {
  cat("  ⚠️ Targets with errors:\n")
  print(errors)
} else {
  cat("  ✓ No errors in metadata\n")
}

warnings <- tar_meta(fields = warnings, complete_only = TRUE)
if (nrow(warnings) > 0 && any(!is.na(warnings$warnings))) {
  cat("\n  ⚠️ Targets with warnings:\n")
  print(warnings[!is.na(warnings$warnings), ])
}

cat("\n6. Testing schema functions...\n")
tar_source("R")

# Test that key functions exist
cat("  - calculate_detection_probability: ")
if (exists("calculate_detection_probability")) cat("✓\n") else cat("✗ MISSING\n")

cat("  - normalize_species_summary: ")
if (exists("normalize_species_summary")) cat("✓\n") else cat("✗ MISSING\n")

cat("  - validate_species_summary: ")
if (exists("validate_species_summary")) cat("✓\n") else cat("✗ MISSING\n")

cat("  - validate_relative_cover: ")
if (exists("validate_relative_cover")) cat("✓\n") else cat("✗ MISSING\n")

# Test schema synonyms
cat("\n  - Checking SCHEMA_SYNONYMS:\n")
if (exists("SCHEMA_SYNONYMS")) {
  for (col in names(SCHEMA_SYNONYMS)) {
    cat(sprintf("    %s -> %s\n", col, paste(SCHEMA_SYNONYMS[[col]], collapse = ", ")))
  }
} else {
  cat("    ✗ SCHEMA_SYNONYMS not found\n")
}

cat("\n7. Checking if output directory exists...\n")
output_dir <- Sys.getenv("OUTPUT_DIR", "outputs")
if (dir.exists(output_dir)) {
  files <- list.files(output_dir, pattern = "\\.parquet$")
  if (length(files) > 0) {
    cat(sprintf("  Found %d parquet files:\n", length(files)))
    for (f in files) {
      size <- file.size(file.path(output_dir, f))
      cat(sprintf("    - %s (%.1f MB)\n", f, size / 1024^2))
    }
  } else {
    cat("  No parquet files yet (run tar_make() first)\n")
  }
} else {
  cat(sprintf("  Directory %s doesn't exist yet\n", output_dir))
}

cat("\n=== Test Complete ===\n")
cat("\nNext steps:\n")
cat("  1. Run: targets::tar_make()\n")
cat("  2. Check for errors in the output\n")
cat("  3. If successful, run: source('generate_figures.R')\n\n")
