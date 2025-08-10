# R/run_sample_size_sensitivity_variable.R
run_sample_size_sensitivity_variable <- function(
    fit_result,
    sample_sizes = c(5, 10, 15, 20, 25),
    n_replicates = 3,
    seed = 123,
    thin = 1  # keep every 'thin'-th draw if you ever need to reduce file sizes
) {
  xdata   <- fit_result$fit$xdata
  site_id <- fit_result$site
  pairs   <- year_pairs_consecutive(xdata)
  if (!length(pairs)) {
    return(list(summary = tibble::tibble(), draws = tibble::tibble()))
  }
  
  results <- list()
  draws_index <- list()
  
  # write draws here; one file per K × rep × pair
  dir.create("outputs/draws_variable", showWarnings = FALSE, recursive = TRUE)
  
  for (K in sample_sizes) {
    for (rep in seq_len(n_replicates)) {
      message(glue::glue("🔁 {site_id}: requested {K}, rep {rep}"))
      set.seed(seed + K * 100 + rep)
      
      for (pair in pairs) {
        y  <- strsplit(pair, "_", fixed = TRUE)[[1]]
        y1 <- as.integer(y[1]); y2 <- as.integer(y[2])
        
        avail <- plots_for_pair(xdata, y1, y2)
        n_av  <- length(avail)
        if (!n_av) next
        
        k <- min(K, n_av)  # variable across pairs
        sampled <- sample(avail, size = k, replace = FALSE)
        idx <- which(xdata$plotID %in% sampled)
        
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
          sample_size     = k
        )
        
        # small summary stays in memory
        summary_df <- dplyr::mutate(
          summ$summary,
          replicate              = rep,
          fit_status             = "ok",
          plot_ids               = list(as.character(sampled)),
          year_pair              = pair,
          mode                   = "sensitivity_variable",
          requested_sample_size  = K,
          actual_sample_size     = k,
          coverage_frac          = ifelse(K > 0, k / K, NA_real_)
        )
        key <- paste0("K", K, "_rep", rep, "_", pair)
        results[[key]] <- summary_df
        
        # big draws go to Parquet; we keep only a file index
        dd <- dplyr::mutate(
          summ$draws,
          replicate              = rep,
          fit_status             = "ok",
          plot_ids               = list(as.character(sampled)),
          year_pair              = pair,
          mode                   = "sensitivity_variable",
          requested_sample_size  = K,
          actual_sample_size     = k,
          coverage_frac          = ifelse(K > 0, k / K, NA_real_)
        )
        if (thin > 1 && "draw" %in% names(dd)) {
          dd <- dd[dd$draw %% thin == 0, , drop = FALSE]
        }
        fn <- file.path("outputs/draws_variable",
                        paste0("K", K, "_rep", rep, "_", pair, ".parquet"))
        arrow::write_parquet(dd, fn)
        
        draws_index[[key]] <- tibble::tibble(
          requested_sample_size = K,
          replicate            = rep,
          year_pair            = pair,
          actual_sample_size   = k,
          file                 = fn,
          mode                 = "sensitivity_variable"
        )
        
        # free memory each inner loop
        rm(post_all, post_pair, summ, dd); gc()
      }
    }
  }
  
  list(
    summary = dplyr::bind_rows(results),
    draws   = dplyr::bind_rows(draws_index)  # tibble of file paths
  )
}
