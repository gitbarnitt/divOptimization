# R/run_sample_size_sensitivity.R
run_sample_size_sensitivity <- function(
    fit_result,               # output of fit_gjam_model_test()
    sample_sizes = c(5, 10, 15, 20, 25),
    n_replicates = 3,
    seed = 123,
    thin = 1                  # optional: keep every 'thin'-th draw
) {
  xdata   <- fit_result$fit$xdata
  site_id <- if (!is.null(fit_result$site)) {
    fit_result$site
  } else if ("siteID" %in% names(xdata)) {
    as.character(xdata$siteID[1])
  } else {
    NA_character_
  }
  
  
  all_plots   <- unique(xdata$plotID)
  total_plots <- length(all_plots)
  
  # pairs used to compute per-pair actual N
  pairs <- year_pairs_consecutive(xdata)
  if (!length(pairs)) {
    return(list(summary = tibble::tibble(), draws = tibble::tibble()))
  }
  
  # filter to feasible sizes
  valid_sample_sizes <- sample_sizes[sample_sizes <= total_plots]
  
  results     <- list()  # species-level summaries accumulated in-memory
  draws_index <- list()  # index to parquet files
  
  # one-time dir for fixed-N draws
  draws_root <- Sys.getenv("DRAWS_DIR", "/tmp/draws")
  draws_dir  <- file.path(draws_root, "fixed")
  dir.create(draws_dir, showWarnings = FALSE, recursive = TRUE)
  
  message("[draws] writing shards under: ", draws_dir)
  
  for (size in valid_sample_sizes) {
    for (rep in seq_len(n_replicates)) {
      message(glue::glue("🔁 {site_id}: {size} plots, replicate {rep}"))
      
      set.seed(seed + size * 100 + rep)
      sampled_plots <- sample(all_plots, size)
      
      # simulate per-pair predictions using indices into xdata
      posterior_list <- loop_simulate_changes_with_index(
        fit        = fit_result$fit,
        plot_index = which(xdata$plotID %in% sampled_plots)
      )
      
      # build output per pair
      for (pair in names(posterior_list)) {
        yy <- strsplit(pair, "_", fixed = TRUE)[[1]]
        y1 <- as.integer(yy[1]); y2 <- as.integer(yy[2])
        
        # how many of the sampled_plots are actually available for this pair?
        avail_pair <- plots_for_pair(xdata, y1, y2)
        used_plots <- intersect(avail_pair, sampled_plots)
        k          <- length(used_plots)
        
        post_pair <- posterior_list[[pair]]
        if (is.null(post_pair) || !k) next
        
        summ <- calculate_detection_probability(
          posterior_preds = post_pair,
          year_pair       = c(y1, y2),
          site_id         = site_id,
          sample_size     = k
        )
        
        # --- small species-level summary (in memory) ---
        summary_df <- dplyr::mutate(
          summ$summary,
          site                  = if ("site" %in% names(summ$summary)) .data$site else site_id,
          replicate             = rep,
          fit_status            = "ok",
          plot_ids              = list(as.character(used_plots)),
          year_pair             = pair,
          mode                  = "sensitivity_fixed",
          requested_sample_size = size,
          actual_sample_size    = k,
          coverage_frac         = ifelse(size > 0, k / size, NA_real_)
        )
        key <- paste0("size", size, "_rep", rep, "_", pair)
        results[[key]] <- summary_df
        
        # --- big per-draw outputs -> Parquet, keep only index in memory ---
        dd <- dplyr::mutate(
          summ$draws,
          site                  = site_id,
          replicate             = rep,
          fit_status            = "ok",
          plot_ids              = list(as.character(used_plots)),
          year_pair             = pair,
          mode                  = "sensitivity_fixed",
          requested_sample_size = size,
          actual_sample_size    = k,
          coverage_frac         = ifelse(size > 0, k / size, NA_real_)
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
        
        fn <- file.path(draws_dir, paste0("size", size, "_rep", rep, "_", pair, ".parquet"))
        
        #fn <- file.path("outputs/draws_fixed",
        #                paste0("size", size, "_rep", rep, "_", pair, ".parquet"))
        arrow::write_parquet(dd, fn)
        
        draws_index[[key]] <- tibble::tibble(
          site                  = site_id,
          requested_sample_size = size,
          actual_sample_size    = k,
          replicate             = rep,
          year_pair             = pair,
          file                  = fn,
          mode                  = "sensitivity_fixed"
        )
        
        rm(summ, dd); gc()
      }
      
      rm(posterior_list); gc()
    }
  }
  
  # --- STANDARDIZE SUMMARY SCHEMA (canonical names) ---
  summary_all <- dplyr::bind_rows(results)
  
  if (!nrow(summary_all)) {
    return(list(summary = tibble::tibble(), draws = dplyr::bind_rows(draws_index)))
  }
  
  if (!"site" %in% names(summary_all)) summary_all$site <- site_id
  
  # Ensure mean_detection exists (it should from calculate_detection_probability)
  if (!"mean_detection" %in% names(summary_all)) {
    stop("run_sample_size_sensitivity(): missing mean_detection column.", call. = FALSE)
  }
  
  # normalize sample_size (prefer actual per-pair; otherwise requested)
  if (!"sample_size" %in% names(summary_all)) {
    if ("actual_sample_size" %in% names(summary_all)) {
      summary_all$sample_size <- summary_all$actual_sample_size
    } else if ("requested_sample_size" %in% names(summary_all)) {
      summary_all$sample_size <- summary_all$requested_sample_size
    } else {
      stop("run_sample_size_sensitivity(): missing sample_size/actual/requested.", call. = FALSE)
    }
  }
  
  if (!"replicate" %in% names(summary_all)) summary_all$replicate <- 1L
  for (nm in c("fit_status","plot_ids")) {
    if (!nm %in% names(summary_all)) summary_all[[nm]] <- NA_character_
  }
  
  # REQUIRED downstream
  req <- c("site","sample_size","species","year_baseline","year_changed",
           "mean_detection","replicate","fit_status","plot_ids")
  miss <- setdiff(req, names(summary_all))
  if (length(miss)) {
    stop(sprintf(
      "run_sample_size_sensitivity(): missing columns after standardization: %s",
      paste(miss, collapse = ", ")
    ), call. = FALSE)
  }
  
  # niceness: canonical first; preserve extras
  summary_all <- summary_all[, unique(c(req, names(summary_all))), drop = FALSE]
  
  draws_all <- dplyr::bind_rows(draws_index)
  
  list(summary = summary_all, draws = draws_all)
}
