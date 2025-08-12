# R/run_baseline_full_available.R
run_baseline_full_available <- function(fit_result, seed = 123) {
  xdata   <- fit_result$fit$xdata
  site_id <- fit_result$site
  pairs   <- year_pairs_consecutive(xdata)
  if (!length(pairs)) return(list(summary = tibble::tibble(), draws = tibble::tibble()))
  
  results <- list(); draws_all <- list()
  set.seed(seed)
  
  # Create output directory ONCE (before the loop)
  dir.create("outputs/draws_baseline", showWarnings = FALSE, recursive = TRUE)
  
  
  for (pair in pairs) {
    y <- strsplit(pair, "_", fixed = TRUE)[[1]]
    y1 <- as.integer(y[1]); y2 <- as.integer(y[2])
    avail <- plots_for_pair(xdata, y1, y2)
    n_av  <- length(avail)
    if (!n_av) next
    
    idx <- which(xdata$plotID %in% avail)
    post_all <- loop_simulate_changes_with_index(
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
    
    summary_df <- dplyr::mutate(
      summ$summary,
      replicate              = 1L,
      fit_status             = "ok",
      plot_ids               = list(as.character(avail)),
      year_pair              = pair,
      mode                   = "baseline_full",
      requested_sample_size  = n_av,   # equals actual here
      actual_sample_size     = n_av
    )
    
    # NEW: write draws to Parquet and record the file path (avoid huge in-memory bind)
    dd <- dplyr::mutate(
      summ$draws,
      replicate              = 1L,
      fit_status             = "ok",
      plot_ids               = list(as.character(avail)),
      year_pair              = pair,
      mode                   = "baseline_full",
      requested_sample_size  = n_av,
      actual_sample_size     = n_av
    )
    
    # -- NEW: ensure a continuous difference column for plotting --
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
    # ------------------------------------------------------------
    
    fn <- file.path("outputs/draws_baseline", paste0("baseline_", pair, ".parquet"))
    arrow::write_parquet(dd, fn)
    
    key <- paste0("baseline_", pair)
    results[[key]] <- summary_df
    # store an index row, not the whole draws df
    draws_all[[key]] <- tibble::tibble(
      year_pair = pair,
      file      = fn,
      mode      = "baseline_full"
    )
    
    # free temps to keep RAM down
    rm(post_all, post_pair, summ, dd); gc()
  }
  
  list(summary = dplyr::bind_rows(results),
       draws   = dplyr::bind_rows(draws_all))
}
