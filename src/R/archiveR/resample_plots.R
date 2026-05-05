#' Resample plotIDs for Tier 2A bootstrap uncertainty
#'
#' @param xdata data.frame with plotID, year, and covariates
#' @param ydata data.frame with plotID, year, and species columns
#' @param sample_frac fraction of unique plots to sample (default 0.8)
#' @param seed random seed for reproducibility
#' @param replacement sample with replacement (TRUE for bootstrap, FALSE for subsampling)
#'
#' @return list with:
#'   - xdata_resampled: resampled xdata (all years for sampled plots)
#'   - ydata_resampled: resampled ydata (matching rows)
#'   - sampled_plots: character vector of plotIDs included
#'   - n_plots_unique: number of unique plots in resample
#'
#' @details
#' - Resamples at plotID level (not row level)
#' - Includes all years for each sampled plot
#' - Preserves row alignment between xdata and ydata
#' - Deterministic given seed
resample_plots <- function(xdata, ydata, sample_frac = 0.8, seed = NULL, replacement = TRUE) {
  
  # Input validation
  stopifnot(
    is.data.frame(xdata),
    is.data.frame(ydata),
    nrow(xdata) == nrow(ydata),
    "plotID" %in% names(xdata),
    "plotID" %in% names(ydata),
    "year" %in% names(xdata),
    "year" %in% names(ydata),
    sample_frac > 0 && sample_frac <= 1.5  # allow > 1 for oversampling with replacement
  )
  
  # Check row alignment
  if (!identical(xdata$plotID, ydata$plotID) || !identical(xdata$year, ydata$year)) {
    stop("resample_plots(): xdata and ydata plotID/year must match exactly")
  }
  
  # Get unique plots
  unique_plots <- unique(xdata$plotID)
  n_unique <- length(unique_plots)
  
  if (n_unique == 0) {
    stop("resample_plots(): no plots found in xdata")
  }
  
  # Determine sample size
  n_sample <- max(1, round(n_unique * sample_frac))
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Resample plotIDs (m-out-of-n bootstrap if replacement=TRUE)
  sampled_plots <- sample(unique_plots, size = n_sample, replace = replacement)
  
  # Count unique plots vs total sampled (matters for bootstrap with replacement)
  n_plots_unique_sampled <- length(unique(sampled_plots))
  n_duplicates <- n_sample - n_plots_unique_sampled
  
  # CRITICAL: Handle duplicates properly for bootstrap
  # If replacement=TRUE and we got duplicates, we need to replicate those plot-year rows
  # to maintain the bootstrap sample size
  if (replacement && n_duplicates > 0) {
    # Build expanded dataset by replicating plot-year rows for duplicates
    # This preserves bootstrap semantics (m observations, some duplicated)
    plot_counts <- table(sampled_plots)
    
    idx_list <- lapply(names(plot_counts), function(pid) {
      base_idx <- which(xdata$plotID == pid)
      rep(base_idx, times = plot_counts[[pid]])  # replicate rows
    })
    idx <- unlist(idx_list)
    
    message(sprintf("[resample_plots] Bootstrap: sampled %d plots (%d unique, %d duplicates)",
                    n_sample, n_plots_unique_sampled, n_duplicates))
  } else {
    # No duplicates (either replacement=FALSE or got lucky with replacement=TRUE)
    idx <- which(xdata$plotID %in% sampled_plots)
    
    if (!replacement) {
      message(sprintf("[resample_plots] Subsample (no replacement): sampled %d unique plots",
                      n_plots_unique_sampled))
    }
  }
  
  if (length(idx) == 0) {
    stop("resample_plots(): resampling resulted in 0 rows")
  }
  
  xdata_resampled <- xdata[idx, , drop = FALSE]
  ydata_resampled <- ydata[idx, , drop = FALSE]
  
  # CRITICAL: Add bootstrap replicate ID to ensure row uniqueness
  # When duplicates occur, (plotID, year) is no longer a unique key
  # boot_rep_id makes each row unique for downstream pivots/joins
  xdata_resampled$boot_rep_id <- seq_len(nrow(xdata_resampled))
  ydata_resampled$boot_rep_id <- seq_len(nrow(ydata_resampled))
  
  # Rebuild row names
  rownames(xdata_resampled) <- NULL
  rownames(ydata_resampled) <- NULL
  
  # Return
  list(
    xdata_resampled = xdata_resampled,
    ydata_resampled = ydata_resampled,
    sampled_plots = as.character(sampled_plots),  # may contain duplicates
    n_plots_unique = n_plots_unique_sampled,      # unique plotIDs sampled
    n_plots_total = n_unique,                     # total available
    n_duplicates = n_duplicates,                  # how many duplicates
    n_rows_original = nrow(xdata),                # original row count
    n_rows_resampled = nrow(xdata_resampled),     # resampled row count
    sample_frac = sample_frac,
    replacement = replacement,
    seed = seed,
    has_boot_rep_id = TRUE  # flag that boot_rep_id column was added
  )
}
