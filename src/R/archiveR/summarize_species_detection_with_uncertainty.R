# R/summarize_species_detection_with_uncertainty.R
summarize_species_detection_with_uncertainty <- function(
    summary_df,  # results$summary
    draws_df     # results$draws OR an index tibble with `file`
) {
  # --- species point summaries ---
  species_summary <- summary_df %>%
    dplyr::mutate(
      year_baseline = as.integer(.data$year_baseline),
      year_changed  = as.integer(.data$year_changed),
      year_pair     = dplyr::coalesce(.data$year_pair,
                                      paste0(.data$year_baseline, "_", .data$year_changed))
    ) %>%
    dplyr::relocate(mean_detection, .after = year_pair)
  
  # If no draws, return NA CIs (unchanged behavior)
  if (is.null(draws_df) || !is.data.frame(draws_df) || !nrow(draws_df)) {
    return(dplyr::mutate(species_summary, ci_lower = NA_real_, ci_upper = NA_real_))
  }
  
  # BRANCH A: full draws in-memory with `detected` (unchanged, rarely used)
  if ("detected" %in% names(draws_df)) {
    ci_draws <- draws_df %>%
      dplyr::mutate(
        year_baseline = as.integer(.data$year_baseline),
        year_changed  = as.integer(.data$year_changed)
      ) %>%
      dplyr::group_by(site, sample_size, species, year_baseline, year_changed, replicate) %>%
      dplyr::summarise(
        ci_lower = stats::quantile(detected, 0.025, na.rm = TRUE),
        ci_upper = stats::quantile(detected, 0.975, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      dplyr::mutate(year_pair = paste0(year_baseline, "_", year_changed))
    
  } else if ("file" %in% names(draws_df)) {
    # BRANCH B: index of parquet files -> read only needed cols; summarise; bind
    if (!requireNamespace("arrow", quietly = TRUE)) {
      warning("Arrow is not available; returning NA CIs.")
      return(dplyr::mutate(species_summary, ci_lower = NA_real_, ci_upper = NA_real_))
    }
    
    .ensure_sample_size <- function(df, meta = NULL) {
      if (!"sample_size" %in% names(df)) {
        if ("actual_sample_size" %in% names(df)) {
          df$sample_size <- df$actual_sample_size
        } else if ("requested_sample_size" %in% names(df)) {
          df$sample_size <- df$requested_sample_size
        } else if (!is.null(meta)) {
          if ("actual_sample_size" %in% names(meta)) {
            df$sample_size <- meta$actual_sample_size
          } else if ("requested_sample_size" %in% names(meta)) {
            df$sample_size <- meta$requested_sample_size
          } else {
            df$sample_size <- NA_real_
          }
        } else {
          df$sample_size <- NA_real_
        }
      }
      df
    }
    
    per_file <- vector("list", nrow(draws_df))
    for (i in seq_len(nrow(draws_df))) {
      meta <- draws_df[i, , drop = FALSE]
      
      # ---- CHANGE 1: read ONLY the columns we need from parquet ----
      needed <- c(
        "site","sample_size","species","year_baseline","year_changed",
        "replicate","detected","actual_sample_size","requested_sample_size"
      )
      dt <- tryCatch(
        arrow::read_parquet(
          meta$file,
          as_data_frame = TRUE,
          col_select    = tidyselect::any_of(needed)
        ),
        error = function(e) {
          warning(sprintf("Could not read parquet: %s (%s)", meta$file, conditionMessage(e)))
          return(NULL)
        }
      )
      if (is.null(dt) || !nrow(dt)) { per_file[[i]] <- tibble::tibble(); next }
      
      # Fill keys from the index if missing in the shard
      if (!"site" %in% names(dt)      && "site" %in% names(meta))      dt$site      <- meta$site
      if (!"replicate" %in% names(dt) && "replicate" %in% names(meta)) dt$replicate <- meta$replicate
      dt <- .ensure_sample_size(dt, meta)
      
      # Minimal required columns
      need <- c("site","sample_size","species","year_baseline","year_changed","replicate","detected")
      if (length(setdiff(need, names(dt)))) { per_file[[i]] <- tibble::tibble(); next }
      
      dt$year_baseline <- as.integer(dt$year_baseline)
      dt$year_changed  <- as.integer(dt$year_changed)
      # ---- CHANGE 2: cast once, then sum() ----
      dt$detected      <- as.integer(dt$detected)
      
      # Reduce to tiny counts for CI
      ci_i <- dt %>%
        dplyr::group_by(.data$site, .data$sample_size, .data$species,
                        .data$year_baseline, .data$year_changed, .data$replicate) %>%
        dplyr::summarise(
          n_draws  = dplyr::n(),
          n_detect = sum(.data$detected, na.rm = TRUE),
          .groups  = "drop"
        ) %>%
        dplyr::mutate(
          ci_lower  = stats::qbeta(0.025, n_detect + 1, n_draws - n_detect + 1),
          ci_upper  = stats::qbeta(0.975, n_detect + 1, n_draws - n_detect + 1),
          year_pair = paste0(.data$year_baseline, "_", .data$year_changed)
        ) %>%
        dplyr::select(dplyr::any_of(c(
          "site","species","year_baseline","year_changed","replicate","sample_size","year_pair",
          "ci_lower","ci_upper"
        )))
      
      per_file[[i]] <- ci_i
      rm(dt, ci_i); gc()
    }
    
    ci_draws <- dplyr::bind_rows(per_file)
    
  } else {
    # Unknown draws shape: return NA CIs rather than erroring
    return(dplyr::mutate(species_summary, ci_lower = NA_real_, ci_upper = NA_real_))
  }
  
  # Join CIs to point summaries
  join_keys <- intersect(
    c("site", "sample_size","species","year_baseline","year_changed","replicate","year_pair"),
    names(species_summary)
  )
  result <- species_summary %>%
    dplyr::left_join(ci_draws, by = join_keys)
  
  if (!"ci_lower" %in% names(result)) result$ci_lower <- NA_real_
  if (!"ci_upper" %in% names(result)) result$ci_upper <- NA_real_
  
  result
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
