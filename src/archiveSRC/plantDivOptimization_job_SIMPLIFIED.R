#!/usr/bin/env Rscript

##############################################################################################
#' @title Plant Diversity Optimization - GCP Job Runner
#' @author Dave Barnett
#' @description Simplified GCP job runner - builds parquet files, uploads to GCS
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

### Authenticate to Google
message("Authenticating to GCS...")
myToken <- gargle::token_fetch(scope = "https://www.googleapis.com/auth/cloud-platform")
googleCloudStorageR::gcs_auth(token = myToken)
bucket_name <- "neon-dev-os-data-availability"
gcs_global_bucket(bucket_name)
message("GCS authenticated, bucket: ", bucket_name)

### Download input data
local_input_dir  <- "/tmp/input"
local_input_path <- file.path(local_input_dir, "plant_data.rds")
dir.create(local_input_dir, recursive = TRUE, showWarnings = FALSE)

message("Downloading input RDS from GCS...")
googleCloudStorageR::gcs_get_object(
  object = "div_optimization/input/plant_data.rds",
  saveToDisk = local_input_path,
  overwrite = TRUE
)
if (!file.exists(local_input_path)) stop("Input RDS download failed")
message("Downloaded: ", local_input_path, " (",  format(file.info(local_input_path)$size, big.mark=","), " bytes)")

### Set environment variables
Sys.setenv(INPUT_RDS_PATH = local_input_path)
Sys.setenv(GJAM_QUICK = Sys.getenv("GJAM_QUICK", "true"))
Sys.setenv(OUTPUT_DIR = "/tmp/outputs")
Sys.setenv(DRAWS_DIR = "/tmp/draws")
Sys.setenv(PRUNE_MODE = Sys.getenv("PRUNE_MODE", "conservative"))
dir.create(Sys.getenv("DRAWS_DIR"), recursive = TRUE, showWarnings = FALSE)

message("Env: INPUT_RDS_PATH=", Sys.getenv("INPUT_RDS_PATH"))
message("Env: GJAM_QUICK=", Sys.getenv("GJAM_QUICK"))
message("Env: PRUNE_MODE=", Sys.getenv("PRUNE_MODE"))

### Run targets pipeline
message("Running targets pipeline...")
store_dir <- "/tmp/_targets"
dir.create(store_dir, recursive = TRUE, showWarnings = FALSE)

ok <- FALSE
err_msg <- NULL
tryCatch({
  targets::tar_make(
    script         = "_targets.R",
    store          = store_dir,
    reporter       = "timestamp",
    callr_function = NULL
  )
  ok <- TRUE
}, error = function(e) {
  err_msg <<- conditionMessage(e)
  message("tar_make() error: ", err_msg)
})

if (!ok) {
  stop("Pipeline failed: ", if (!is.null(err_msg)) err_msg else "Unknown error")
}

message("✓ Pipeline completed successfully")

### Optional: Generate figures (controlled by GENERATE_FIGURES env var)
if (tolower(Sys.getenv("GENERATE_FIGURES", "false")) == "true") {
  message("\n=== Generating figures ===")
  tryCatch({
    source("generate_figures.R")
    message("✓ Figures generated successfully")
  }, error = function(e) {
    warning("Figure generation failed (data files are still OK): ", conditionMessage(e))
  })
} else {
  message("(Skipping figure generation - set GENERATE_FIGURES=true to enable)")
}

### Verify key outputs exist
output_dir <- Sys.getenv("OUTPUT_DIR", "/tmp/outputs")
key_files <- c("species_summary.parquet", "draws_index.parquet", 
               "relative_cover.parquet", "schema_report.csv")

for (fname in key_files) {
  fpath <- file.path(output_dir, fname)
  if (!file.exists(fpath)) {
    warning("Expected output missing: ", fname)
  } else {
    sz <- file.info(fpath)$size
    message("✓ ", fname, " (", format(sz, big.mark=","), " bytes)")
  }
}

### Collect all output files
output_files <- list.files(output_dir, full.names = TRUE, recursive = TRUE)
if (length(output_files) == 0) {
  stop("No output files found in: ", output_dir)
}
message("\nFound ", length(output_files), " files to upload")

### Upload to GCS
exec_id     <- Sys.getenv("CLOUD_RUN_EXECUTION", format(Sys.time(), "%Y%m%d-%H%M%S"))
dest_prefix <- file.path("div_optimization", "output", exec_id)

message("Uploading to gs://", bucket_name, "/", dest_prefix, "/")

upload_count <- 0
for (f in output_files) {
  rel_path <- sub(paste0("^", output_dir, "/?"), "", f)
  dest_obj <- file.path(dest_prefix, rel_path)
  
  # Retry upload up to 3 times with exponential backoff
  max_retries <- 3
  uploaded <- FALSE
  
  for (attempt in 1:max_retries) {
    res <- try(
      googleCloudStorageR::gcs_upload(
        file        = f,
        name        = dest_obj,
        upload_type = "simple"
      ),
      silent = TRUE
    )
    
    if (!inherits(res, "try-error") && !is.null(res) && !is.null(res$name)) {
      upload_count <- upload_count + 1
      if (attempt > 1) {
        message("  ✓ ", rel_path, " (succeeded on attempt ", attempt, ")")
      } else {
        message("  ✓ ", rel_path)
      }
      uploaded <- TRUE
      break
    } else {
      if (attempt < max_retries) {
        wait_sec <- 2^attempt  # 2, 4, 8 seconds
        message("  ⟳ ", rel_path, " failed (attempt ", attempt, "/", max_retries, "), retrying in ", wait_sec, "s...")
        Sys.sleep(wait_sec)
      }
    }
  }
  
  if (!uploaded) {
    warning("  ✗ Upload failed after ", max_retries, " attempts: ", rel_path)
  }
}

if (upload_count == 0) {
  stop("All uploads failed!")
}

message("\n✓ Upload complete: ", upload_count, "/", length(output_files), " files")

### List uploaded objects
Sys.sleep(2)  # Brief wait for GCS consistency
lst <- try(googleCloudStorageR::gcs_list_objects(prefix = dest_prefix), silent = TRUE)

if (!inherits(lst, "try-error") && is.data.frame(lst) && nrow(lst)) {
  message("\nUploaded objects:")
  print(lst[, c("name","size","updated")])
} else {
  message("\n(Object listing temporarily unavailable)")
}

message("\n✓✓✓ Job completed successfully ✓✓✓")
message("Results: gs://", bucket_name, "/", dest_prefix, "/")
