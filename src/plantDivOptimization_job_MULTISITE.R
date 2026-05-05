#!/usr/bin/env Rscript

##############################################################################################
#' @title Plant Diversity Optimization - GCP Multi-Site Job Runner
#' @author Dave Barnett
#' @description Single-site runner for parallel deployment across 47 sites
#' @usage Set SITE_ID environment variable to run specific site
##############################################################################################

### Load required R libraries ####
message("Loading packages...")
library(gargle)
library(googleCloudStorageR)
library(tidyverse)
library(targets)

options(
  warn = 1,
  error = function(e) {
    cat("FATAL ERROR:", conditionMessage(e), "\n", file = stderr())
    traceback(2)
    quit(status = 1)
  }
)

## Limit thread usage
Sys.setenv(
  ARROW_NUM_THREADS        = "1",
  R_PARALLEL_NUM_THREADS   = "1",
  OMP_NUM_THREADS          = "1",
  MKL_NUM_THREADS          = "1",
  OPENBLAS_NUM_THREADS     = "1"
)

## Set working directory
args <- commandArgs(trailingOnly = FALSE)
script <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script) == 1L) {
  setwd(dirname(normalizePath(script)))
  message("WORKDIR set to: ", getwd())
}

### Get site ID from environment
SITE_ID <- Sys.getenv("SITE_ID", "")
if (SITE_ID == "") {
  stop("ERROR: SITE_ID environment variable not set. Set SITE_ID to run specific site.")
}
message("SITE_ID: ", SITE_ID)

### Authenticate to Google
message("Authenticating to GCS...")
myToken <- gargle::token_fetch(scope = "https://www.googleapis.com/auth/cloud-platform")
googleCloudStorageR::gcs_auth(token = myToken)
bucket_name <- "neon-dev-os-data-availability"
gcs_global_bucket(bucket_name)
message("GCS authenticated, bucket: ", bucket_name)

### Download input data for this site
local_input_dir  <- "/tmp/input"
local_input_path <- file.path(local_input_dir, "plant_data.rds")
dir.create(local_input_dir, recursive = TRUE, showWarnings = FALSE)

message("Downloading input RDS (shared file for all sites) from GCS...")
gcs_input_path <- "div_optimization/input/plant_data.rds"
googleCloudStorageR::gcs_get_object(
  object = gcs_input_path,
  saveToDisk = local_input_path,
  overwrite = TRUE
)
if (!file.exists(local_input_path)) stop("Input RDS download failed")
message("Downloaded: ", local_input_path, " (", format(file.info(local_input_path)$size, big.mark=","), " bytes)")

### Set environment variables
Sys.setenv(INPUT_RDS_PATH = local_input_path)
Sys.setenv(GJAM_QUICK = Sys.getenv("GJAM_QUICK", "false"))  # Default to full run
Sys.setenv(OUTPUT_DIR = "/tmp/outputs")
Sys.setenv(DRAWS_DIR = "/tmp/draws")
Sys.setenv(PRUNE_MODE = Sys.getenv("PRUNE_MODE", "conservative"))

# Clear outputs/draws to prevent contamination from previous runs
for (d in c(Sys.getenv("OUTPUT_DIR"), Sys.getenv("DRAWS_DIR"))) {
  if (dir.exists(d)) {
    message("Clearing ", d, "...")
    unlink(d, recursive = TRUE)
  }
}
dir.create(Sys.getenv("OUTPUT_DIR"), recursive = TRUE, showWarnings = FALSE)
dir.create(Sys.getenv("DRAWS_DIR"), recursive = TRUE, showWarnings = FALSE)

message("=== Environment Variables ===")
message("Env: INPUT_RDS_PATH=", Sys.getenv("INPUT_RDS_PATH"))
message("Env: SITE_ID=", Sys.getenv("SITE_ID"))
message("Env: TIER_MODE=", Sys.getenv("TIER_MODE", "<NOT SET>"))
message("Env: TIER2_REPS=", Sys.getenv("TIER2_REPS", "<NOT SET>"))
message("Env: GJAM_QUICK=", Sys.getenv("GJAM_QUICK"))
message("Env: PRUNE_MODE=", Sys.getenv("PRUNE_MODE"))
message("Env: OUTPUT_DIR=", Sys.getenv("OUTPUT_DIR"))
message("Env: DRAWS_DIR=", Sys.getenv("DRAWS_DIR"))

# TierP0-specific parameters (only show overrideable ones)
if (tolower(Sys.getenv("TIER_MODE", "tier1")) == "tierp0") {
  message("=== TierP0 Parameters ===")
  message("Env: EFFECT=", Sys.getenv("EFFECT", "0.20 (default)"))
  message("Env: POWER_REPS=", Sys.getenv("POWER_REPS", "200 (default)"))
  if (nzchar(Sys.getenv("YEAR_BASELINE", ""))) {
    message("Env: YEAR_BASELINE=", Sys.getenv("YEAR_BASELINE"))
    message("Env: YEAR_PERTURBED=", Sys.getenv("YEAR_PERTURBED"))
  } else {
    message("Years: auto-detect consecutive pairs")
  }
  message("(N_GRID, NOISE_MODE, DECISION_RULE are hard-coded in tierp0_analysis.R)")
  message("=========================")
}
message("=============================")


### Run orchestration based on TIER_MODE
message("Running orchestration for ", SITE_ID, "...")
store_dir <- "/tmp/_targets"

# Clear any cached _targets store to prevent mode conflicts
if (dir.exists(store_dir)) {
  message("Removing cached _targets store to prevent stale pipeline definitions...")
  unlink(store_dir, recursive = TRUE)
}
dir.create(store_dir, recursive = TRUE, showWarnings = FALSE)

# Parse check to catch syntax errors early (before tar_make)
targets_path <- "_targets.R"
message("Parsing targets file: ", targets_path)

# File identity verification (proves which version is deployed)
targets_abs_path <- normalizePath(targets_path, mustWork = FALSE)
targets_md5 <- tools::md5sum(targets_path)
message("  Resolved path: ", targets_abs_path)
message("  MD5 checksum: ", targets_md5)

# Check TIER_MODE for orchestration
tier_mode <- tolower(Sys.getenv("TIER_MODE", "tier1"))

if (tier_mode == "tierp0") {
  message("\n========================================")
  message("TierP0 MODE: Using tierp0_analysis.R")
  message("========================================")
  
  tierp0_script <- "tierp0_analysis.R"
  if (!file.exists(tierp0_script)) {
    stop("TierP0 script not found: ", tierp0_script)
  }
  
  # Run the TierP0 analysis script
  source(tierp0_script)

} else if (tier_mode == "trendrun") {
  message("\n========================================")
  message("TrendRun MODE: Using trendrun_analysis.R")
  message("========================================")

  trendrun_script <- "trendrun_analysis.R"
  if (!file.exists(trendrun_script)) {
    stop("TrendRun script not found: ", trendrun_script)
  }

  # Ensure TREND_SIZES is set (default to 0.20 if not provided)
  # Backward compatibility: Also check TREND_ADD_NET
  if (Sys.getenv("TREND_SIZES", "") == "" && Sys.getenv("TREND_ADD_NET", "") == "") {
    Sys.setenv(TREND_SIZES = "0.20")
  }

  # Run the TrendRun analysis script
  source(trendrun_script)

} else {
  # Tier1 or Tier2A: Use targets framework
  message("\n========================================")
  message("TIER1/TIER2A MODE: Using targets framework")
  message("========================================")
  
  parse_result <- tryCatch(
  {
    parse(file = targets_path, keep.source = TRUE)
    TRUE
  },
  error = function(e) {
    cat("✗ Parse FAILED:", targets_path, "\n", file = stderr())
    cat(conditionMessage(e), "\n", file = stderr())
    quit(save = "no", status = 1)
  }
)
  message("✓ Parse OK: ", targets_path)
  
  targets::tar_make(
    script      = "_targets.R",
    store       = store_dir,
    reporter    = "timestamp",
    callr_function = NULL,  # Run in-process for debugging
    envir       = globalenv()
  )
  
  message("Pipeline complete for ", SITE_ID)
}

# Verify outputs were produced
site_out <- file.path(Sys.getenv("OUTPUT_DIR"), SITE_ID)
output_files <- list.files(site_out, recursive = TRUE, full.names = TRUE)

if (length(output_files) == 0) {
  cat("✗ No outputs were produced in ", site_out, "\n", file = stderr())
  quit(save = "no", status = 1)
}

message("✓ Produced ", length(output_files), " output files")


### Upload outputs to GCS
message("Uploading outputs for ", SITE_ID, " to GCS...")
output_dir <- Sys.getenv("OUTPUT_DIR")
output_files <- list.files(output_dir, pattern = "\\.parquet$|\\.csv$|\\.rds$", full.names = TRUE, recursive = TRUE)

if (length(output_files) == 0) {
  warning("No output files found in ", output_dir)
} else {
  for (f in output_files) {
    # Preserve subdirectory structure (e.g., tierp0/, tier2/)
    # Guard against path canonicalization mismatches
    root <- normalizePath(output_dir, winslash = "/")
    full <- normalizePath(f, winslash = "/")
    
    if (!startsWith(full, root)) {
      stop(sprintf("Output file path is not under OUTPUT_DIR: %s (root: %s)", full, root))
    }
    
    rel_path <- sub(paste0("^", root, "/?"), "", full)
    fname <- rel_path  # For logging (preserves subfolder context)
    # GCS path: rel_path already includes SITE_ID/tierp0/ or SITE_ID/trendrun/ structure
    # Version-aware output routing to preserve old results during methodology changes
    pipeline_version <- Sys.getenv("PIPELINE_VERSION", "ygibbs")  # default to new version
    if (tier_mode == "trendrun") {
      gcs_base <- "div_optimization/outputs_trendrun/"
    } else if (tier_mode == "tierp0") {
      gcs_base <- sprintf("div_optimization/outputs_%s/", pipeline_version)
    } else {
      gcs_base <- "div_optimization/outputs/"  # tier1/tier2a unchanged
    }
    gcs_path <- paste0(gcs_base, rel_path)
    
    # Retry upload up to 3 times with exponential backoff
    max_retries <- 3
    uploaded <- FALSE
    
    for (attempt in 1:max_retries) {
      res <- try(
        googleCloudStorageR::gcs_upload(
          file = f,
          bucket = bucket_name,
          name = gcs_path,
          predefinedAcl = "bucketLevel"
        ),
        silent = TRUE
      )
      
      if (!inherits(res, "try-error")) {
        if (attempt > 1) {
          message("  ✓ ", fname, " (succeeded on attempt ", attempt, ")")
        } else {
          message("  ✓ ", fname)
        }
        uploaded <- TRUE
        break
      } else {
        if (attempt < max_retries) {
          wait_sec <- 2^attempt
          message("  ⟳ ", fname, " failed (attempt ", attempt, "/", max_retries, "), retrying in ", wait_sec, "s...")
          Sys.sleep(wait_sec)
        }
      }
    }
    
    if (!uploaded) {
      warning("  ✗ Upload failed after ", max_retries, " attempts: ", fname)
    }
  }
}

### Optional: Upload draws files if they exist
draws_dir <- Sys.getenv("DRAWS_DIR")
if (dir.exists(draws_dir)) {
  draws_files <- list.files(draws_dir, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)
  
  if (length(draws_files) > 0) {
    message("Found ", length(draws_files), " draw files - uploading all")
    
    for (f in draws_files) {
      fname <- basename(f)
      # Version-aware draws upload (matches main output routing)
      pipeline_version <- Sys.getenv("PIPELINE_VERSION", "ygibbs")
      if (tier_mode == "trendrun") {
        gcs_base <- "div_optimization/outputs_trendrun/"
      } else if (tier_mode == "tierp0") {
        gcs_base <- sprintf("div_optimization/outputs_%s/", pipeline_version)
      } else {
        gcs_base <- "div_optimization/outputs/"
      }
      gcs_path <- paste0(gcs_base, SITE_ID, "/draws/", fname)
      message("Uploading draw example: ", fname)
      
      tryCatch({
        googleCloudStorageR::gcs_upload(
          file = f,
          bucket = bucket_name,
          name = gcs_path,
          predefinedAcl = "bucketLevel"
        )
      }, error = function(e) {
        warning("  ✗ Failed to upload draw file: ", conditionMessage(e))
      })
    }
    message("Uploaded ", length(draws_files), " draw files")
  }
}

message("===========================================")
message("SUCCESS: ", SITE_ID, " processing complete")
message("===========================================")
