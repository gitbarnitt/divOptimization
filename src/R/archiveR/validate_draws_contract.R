validate_draws_contract <- function(draws_idx, take = 3L) {
  if (is.null(draws_idx) || !NROW(draws_idx)) stop("Empty draws index.")
  if (!"file" %in% names(draws_idx)) stop("Draws index missing 'file' column.")    # NEW
  if (!requireNamespace("arrow", quietly = TRUE)) stop("arrow not installed.")
  
  files <- unique(draws_idx$file)
  files <- files[seq_len(min(length(files), take))]
  if (!length(files)) stop("Index has no 'file' paths.")
  
  need <- c("site","species","year_baseline","year_changed","replicate","detected","draw")
  ok_any_size <- c("sample_size","requested_sample_size","actual_sample_size")
  
  for (fp in files) {
    if (is.na(fp) || !nzchar(fp)) stop("Index contains blank 'file' path.")        # NEW
    # arrow::read_parquet will also fail if the file doesn't exist, which is fine
    dt <- tryCatch(
      arrow::read_parquet(
        fp, as_data_frame = TRUE,
        col_select = tidyselect::any_of(c(need, ok_any_size))
      ),
      error = function(e) stop("Parquet read failed: ", fp, " :: ", conditionMessage(e))
    )
    
    miss <- setdiff(need, names(dt))
    if (length(miss)) stop("Shard missing: ", paste(miss, collapse=", "), " in ", fp)
    
    if (!any(ok_any_size %in% names(dt))) stop("No sample size column in shard: ", fp)
    
    # --- allow logical detected; coerce for validation only ---
    if (is.logical(dt$detected)) dt$detected <- as.integer(dt$detected)            # NEW
    if (!is.numeric(dt$detected)) stop("'detected' not numeric/logical in shard: ", fp)
    
    bad <- unique(dt$detected[!is.na(dt$detected) & !dt$detected %in% c(0,1)])
    if (length(bad)) stop("Non {0,1} 'detected' values in ", fp)
  }
  
  message("[validate_draws_contract] ok for ", length(files), " shard(s).")
  invisible(TRUE)
}
