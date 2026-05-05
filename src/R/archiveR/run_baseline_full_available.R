# R/run_baseline_full_available.R
run_baseline_full_available <- function(fit_result, seed = 123, thin = 1) {
  # Inputs
  xdata <- fit_result$fit$xdata
  site_id <- if (!is.null(fit_result$site)) {
    fit_result$site
  } else if ("siteID" %in% names(xdata)) {
    as.character(xdata$siteID[1])
  } else {
    NA_character_
  }
  
  pairs <- year_pairs_consecutive(xdata)
  if (!length(pairs)) {
    return(list(summary = tibble::tibble(), draws = tibble::tibble()))
  }
  
  # Outputs we accumulate
  results     <- list()
  draws_index <- list()
  
  # One-time setup
  set.seed(seed)
  
  # Use DRAWS_DIR environment variable (same pattern as variable sensitivity)
  draws_root <- Sys.getenv("DRAWS_DIR", "outputs/draws")
  draws_dir  <- file.path(draws_root, "baseline")
  dir.create(draws_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Loop all year pairs
  for (pair in pairs) {
    yy <- strsplit(pair, "_", fixed = TRUE)[[1]]
    y1 <- as.integer(yy[1]); y2 <- as.integer(yy[2])
    
    avail <- plots_for_pair(xdata, y1, y2)
    n_av  <- length(avail)
    if (!n_av) next
    
    idx <- which(xdata$plotID %in% avail)
    
    post_all  <- loop_simulate_changes_with_index(
      fit        = fit_result$fit,
      plot_index = idx
    )
    post_pair <- post_all[[pair]]
    if (is.null(post_pair)) next
    
    summ <- calculate_detection_probability(
      posterior_preds = post_pair,
      year_pair       = c(y1, y2),
      site_id         = site_id,
      sample_size     = n_av
    )
    
    # Compact species-level summary in-memory
    summary_df <- dplyr::mutate(
      summ$summary,
      site                   = if ("site" %in% names(summ$summary)) .data$site else site_id,
      replicate              = 1L,
      fit_status             = "ok",
      plot_ids               = list(as.character(avail)),
      year_pair              = pair,
      mode                   = "baseline_full",
      requested_sample_size  = n_av,
      actual_sample_size     = n_av
    )
    key <- paste0("baseline_", pair)
    results[[key]] <- summary_df
    
    # Draws -> Parquet (+ index row only in memory)
    dd <- dplyr::mutate(
      summ$draws,
      site                   = site_id,
      replicate              = 1L,
      fit_status             = "ok",
      plot_ids               = list(as.character(avail)),
      year_pair              = pair,
      mode                   = "baseline_full",
      requested_sample_size  = n_av,
      actual_sample_size     = n_av
    )
    
    # ensure a continuous difference column for plotting
    if (!"diff" %in% names(dd)) {
      if (all(c("pred_changed","pred_baseline") %in% names(dd))) {
        dd$diff <- dd$pred_changed - dd$pred_baseline
      } else if (all(c("mu_changed","mu_baseline") %in% names(dd))) {
        dd$diff <- dd$mu_changed - dd$mu_baseline
      } else if (all(c("y_changed","y_baseline") %in% names(dd))) {
        dd$diff <- dd$y_changed - dd$y_baseline
      } else if (all(c("mean_changed","mean_baseline") %in% names(dd))) {
        dd$diff <- dd$mean_changed - dd$mean_baseline
      } else {
        dd$diff <- NA_real_
      }
    }
    
    if (thin > 1 && "draw" %in% names(dd)) {
      dd <- dd[dd$draw %% thin == 0, , drop = FALSE]
    }
    
    fn <- file.path(draws_dir, paste0("baseline_", pair, ".parquet"))
    arrow::write_parquet(dd, fn)
    
    draws_index[[key]] <- tibble::tibble(
      site                  = site_id,
      year_pair             = pair,
      requested_sample_size = n_av,
      actual_sample_size    = n_av,
      replicate             = 1L,
      file                  = fn,
      mode                  = "baseline_full"
    )
    
    rm(post_all, post_pair, summ, dd); gc()
  }
  
  # ---- STANDARDIZE SUMMARY SCHEMA (canonical names) ----
  summary_all <- dplyr::bind_rows(results)
  
  if (!"site" %in% names(summary_all)) summary_all$site <- site_id
  
  # Ensure mean_detection exists (it should from calculate_detection_probability)
  if (!"mean_detection" %in% names(summary_all)) {
    stop("run_baseline_full_available(): missing mean_detection column.", call. = FALSE)
  }
  
  # normalize sample_size
  if (!"sample_size" %in% names(summary_all)) {
    if ("actual_sample_size" %in% names(summary_all)) {
      summary_all$sample_size <- summary_all$actual_sample_size
    } else if ("requested_sample_size" %in% names(summary_all)) {
      summary_all$sample_size <- summary_all$requested_sample_size
    } else {
      stop("run_baseline_full_available(): missing sample_size/actual_sample_size/requested_sample_size.", call. = FALSE)
    }
  }
  
  if (!"replicate" %in% names(summary_all)) summary_all$replicate <- 1L
  for (nm in c("fit_status","plot_ids")) {
    if (!nm %in% names(summary_all)) summary_all[[nm]] <- NA_character_
  }
  
  # REQUIRED by downstream
  req <- c("site","sample_size","species","year_baseline","year_changed",
           "mean_detection","replicate","fit_status","plot_ids")
  miss <- setdiff(req, names(summary_all))
  if (length(miss)) {
    stop(sprintf(
      "run_baseline_full_available(): missing columns after standardization: %s",
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }
  
  # niceness: canonical first
  summary_all <- summary_all[, unique(c(req, names(summary_all))), drop = FALSE]
  
  # draws index tibble
  draws_all <- dplyr::bind_rows(draws_index)
  
  list(summary = summary_all, draws = draws_all)
}
