# R/run_sample_size_sensitivity_variable.R
run_sample_size_sensitivity_variable <- function(
    fit_result, sample_sizes = c(5,10,15,20,25), n_replicates = 3, seed = 123, thin = 1
) {
  xdata   <- fit_result$fit$xdata
  site_id <- fit_result$site
  pairs   <- year_pairs_consecutive(xdata)
  if (!length(pairs)) return(list(summary = tibble::tibble(), draws = tibble::tibble()))
  
  results <- list()
  draws_index <- list()
  
  
  # >>> NEW: writable location in Cloud Run
  draws_root <- Sys.getenv("DRAWS_DIR", "/tmp/draws")
  draws_dir  <- file.path(draws_root, "variable")
  dir.create(draws_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("[draws] writing shards under: ", draws_dir)
  
  for (K in sample_sizes) {
    for (rep in seq_len(n_replicates)) {
      set.seed(seed + K * 100 + rep)
      for (pair in pairs) {
        y  <- strsplit(pair, "_", fixed = TRUE)[[1]]
        y1 <- as.integer(y[1]); y2 <- as.integer(y[2])
        
        avail <- plots_for_pair(xdata, y1, y2)
        n_av  <- length(avail)
        if (!n_av) next
        
        k <- min(K, n_av)
        sampled <- sample(avail, size = k, replace = FALSE)
        idx <- which(xdata$plotID %in% sampled)
        
        post_all  <- loop_simulate_changes_with_index(fit = fit_result$fit, plot_index = idx)
        post_pair <- post_all[[pair]]
        if (is.null(post_pair)) next
        
        summ <- calculate_detection_probability(
          posterior_preds = post_pair,
          year_pair       = c(y1, y2),
          site_id         = site_id,
          sample_size     = k
        )
        
        # small summary
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
        
        # >>> NEW: make sure these are present in the shard
        dd <- dplyr::mutate(
          summ$draws,
          site                  = site_id,
          sample_size           = k,
          replicate             = rep,
          fit_status            = "ok",
          plot_ids              = list(as.character(sampled)),
          year_pair             = pair,
          mode                  = "sensitivity_variable",
          requested_sample_size = K,
          actual_sample_size    = k,
          coverage_frac         = ifelse(K > 0, k / K, NA_real_)
        )
        
        # ensure diff present (unchanged)
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
        
        # >>> NEW: write to /tmp
        fn <- file.path(draws_dir, paste0("K", K, "_rep", rep, "_", pair, ".parquet"))
        arrow::write_parquet(dd, fn)
        
        # >>> NEW: include site in index
        draws_index[[key]] <- tibble::tibble(
          site                 = site_id,
          requested_sample_size = K,
          actual_sample_size    = k,
          replicate             = rep,
          year_pair             = pair,
          file                  = fn,
          mode                  = "sensitivity_variable"
        )
        
        rm(post_all, post_pair, summ, dd); gc()
      }
    }
  }
  
  # --- STANDARDIZE OUTPUT SCHEMA BEFORE RETURN --------------------------------
  summary_all <- dplyr::bind_rows(results)
  
  # ensure site exists (some producers may omit; we know site_id)
  if (!"site" %in% names(summary_all)) summary_all$site <- site_id
  
  # Ensure mean_detection exists (it should from calculate_detection_probability)
  if (!"mean_detection" %in% names(summary_all)) {
    stop("run_sample_size_sensitivity_variable(): missing mean_detection column.", call. = FALSE)
  }
  
  if (!"sample_size" %in% names(summary_all)) {
    # prefer actual achieved N, fall back to requested, else error
    if ("actual_sample_size" %in% names(summary_all)) {
      summary_all$sample_size <- summary_all$actual_sample_size
    } else if ("requested_sample_size" %in% names(summary_all)) {
      summary_all$sample_size <- summary_all$requested_sample_size
    } else {
      stop("run_sample_size_sensitivity_variable(): missing sample_size/actual_sample_size/requested_sample_size.", call. = FALSE)
    }
  }
  
  # ensure required fields exist (fill if producer didn't set them)
  # ensure required fields exist (fill if producer didn't set them)
  if (!"fit_status" %in% names(summary_all)) summary_all$fit_status <- NA_character_
  
  if (!"plot_ids" %in% names(summary_all)) {
    # keep it a list-column of character vectors to match producers
    summary_all$plot_ids <- rep(list(character(0)), nrow(summary_all))
  }
  
  # optional: light type coercions for stability
  summary_all$sample_size    <- as.integer(summary_all$sample_size)
  summary_all$year_baseline  <- as.integer(summary_all$year_baseline)
  summary_all$year_changed   <- as.integer(summary_all$year_changed)
  summary_all$mean_detection <- as.numeric(summary_all$mean_detection)
  
  # REQUIRED columns that downstream expects
  req <- c("site","sample_size","species","year_baseline","year_changed",
           "mean_detection","replicate","fit_status","plot_ids")
  missing <- setdiff(req, names(summary_all))
  if (length(missing)) {
    stop(sprintf(
      "run_sample_size_sensitivity_variable(): missing columns after standardization: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  
  # reorder (keep canonical first; retain any extras afterward)
  summary_all <- summary_all[, unique(c(req, names(summary_all))), drop = FALSE]
  
  draws_all <- dplyr::bind_rows(draws_index)  # index of parquet files; schema already OK
  
  list(summary = summary_all, draws = draws_all)
}

