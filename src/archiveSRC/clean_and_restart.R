# clean_and_restart.R
# Script to clean the targets cache and start fresh
# Use this if you want to rebuild everything from scratch

cat("\n=== Cleaning Targets Pipeline ===\n\n")

if (!file.exists("_targets.R")) {
  stop("Please run this script from the src/ directory")
}

library(targets)

cat("1. Checking current pipeline status...\n")
meta <- tar_meta()
cat(sprintf("   Currently %d targets tracked\n", nrow(meta)))

completed <- sum(meta$type == "stem" & !is.na(meta$data), na.rm = TRUE)
cat(sprintf("   %d completed targets\n", completed))

cat("\n2. Cleaning up...\n")

# Remove _targets directory
if (dir.exists("_targets")) {
  cat("   - Removing _targets/ directory...\n")
  unlink("_targets", recursive = TRUE)
}

# Remove outputs directory
if (dir.exists("outputs")) {
  cat("   - Removing outputs/ directory...\n")
  unlink("outputs", recursive = TRUE)
}

# Remove any temp draws directories
if (dir.exists("/tmp/draws")) {
  cat("   - Removing /tmp/draws/ directory...\n")
  unlink("/tmp/draws", recursive = TRUE)
}

cat("\n3. Invalidating all targets...\n")
tar_destroy()

cat("\n✓ Pipeline cleaned!\n\n")
cat("Next steps:\n")
cat("  1. Run: targets::tar_make()\n")
cat("  2. If it works, run: source('generate_figures.R')\n\n")
