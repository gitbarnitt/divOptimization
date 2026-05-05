#' Evaluate Community-Weighted Detection Score
#'
#' Computes the community-weighted mean detection probability for each site, sample size, replicate, and year pair,
#' based on the relative cover of each species.
#'
#' @param sensitivity_results Data frame returned by run_sample_size_sensitivity*()
#' @param relative_cover_df Data frame of relative cover (output of compute_relative_cover())
#' @param draws_df Optional: posterior draw table OR an index (with `file`) produced by write_draws_index()
#'
#' @return A tibble with columns including:
#'   site, year_pair, sample_size, cwm_mean, ci_lower, ci_upper, replicate, fit_status,
#'   and, when available, requested_sample_size, actual_sample_size, n_species.
#'
#' @export
evaluate_community_weighted_detection <- function(
    sensitivity_results,
    relative_cover_df,
    draws_df = NULL
) {
  library(dplyr)
  library(tidyr)
  
  # ---- Input checks ----
  required_summary_cols <- c(
    "site","sample_size","species","year_baseline","year_changed",
    "mean_detection","replicate","fit_status","plot_ids"
  )
  if (!all(required_summary_cols %in% names(sensitivity_results))) {
    stop("❌ sensitivity_results is missing required columns: ",
         paste(setdiff(required_summary_cols, names(sensitivity_results)), collapse = ", "))
  }
  
  required_cover_cols <- c("siteID","plotID","year","taxonID","relative_cover")
  if (!all(required_cover_cols %in% names(relative_cover_df))) {
    stop("❌ relative_cover_df is missing required columns: ",
         paste(setdiff(required_cover_cols, names(relative_cover_df)), collapse = ", "))
  }
  
  # Track optional size labels if provided upstream
  has_req <- "requested_sample_size" %in% names(sensitivity_results)
  has_act <- "actual_sample_size"     %in% names(sensitivity_results)
  
  # ---- Expand plot IDs per replicate ----
  expanded_results <- sensitivity_results %>%
    mutate(
      year_baseline = as.integer(year_baseline),
      year_changed  = as.integer(year_changed)
    ) %>%
    unnest(plot_ids, names_repair = "minimal") %>%
    rename(plotID = plot_ids)
  
  # ---- Relative cover prep ----
  cover_df <- relative_cover_df %>%
    rename(site = siteID, species = taxonID) %>%
    drop_other(taxon_col = "species")  # Exclude "OTHER" from CWM weights
  
  # Join cover for both years at plot level
  expanded_with_cover <- expanded_results %>%
    left_join(cover_df, by = c("site","species","plotID","year_baseline"="year")) %>%
    rename(relative_cover_baseline = relative_cover) %>%
    left_join(cover_df, by = c("site","species","plotID","year_changed"="year")) %>%
    rename(relative_cover_changed = relative_cover) %>%
    mutate(relative_cover_mean = rowMeans(cbind(relative_cover_baseline, relative_cover_changed), na.rm = TRUE)) %>%
    filter(!is.na(relative_cover_mean))
  

  # ✅ Scaffold grouping for aggregation
  # Build grouping columns; include requested/actual if available to avoid collapsing different K
  grp_cols <- c("site","year_baseline","year_changed","replicate","fit_status","sample_size")
  if (has_req) grp_cols <- c("requested_sample_size", grp_cols)
  if (has_act) grp_cols <- c(grp_cols, "actual_sample_size")
  
  # Initial aggregation to get species count (point estimate computed from draws below)
  community_scores <- expanded_with_cover %>%
    group_by(across(all_of(grp_cols))) %>%
    summarise(
      n_species = n_distinct(species),
      .groups = "drop"
    ) %>%
    mutate(year_pair = paste0(year_baseline, "_", year_changed))
  
  # Sensible defaults if not present
  if (!"requested_sample_size" %in% names(community_scores)) {
    community_scores$requested_sample_size <- NA_real_
  }
  if (!"actual_sample_size" %in% names(community_scores)) {
    # By default, treat actual == sample_size when not explicitly tracked
    community_scores$actual_sample_size <- community_scores$sample_size
  }
  
  # ---- Compute point estimate + credible intervals from draws ----
  # NOTE: cwm_mean is the posterior mean of cwm_draw (community-weighted detection per draw).
  # This ensures the point estimate and CIs summarize the same posterior distribution.
  
  if (!is.null(draws_df)) {
    
    # Precompute mean relative cover by site × species × year, normalized within site × year_pair
    avg_cover_df <- relative_cover_df %>%
      dplyr::rename(site = siteID, species = taxonID) %>%
      dplyr::group_by(site, species, year) %>%
      dplyr::summarise(mean_relative_cover = mean(relative_cover, na.rm = TRUE), .groups = "drop")
    
    ci_df <- NULL
    
    if ("detected" %in% names(draws_df)) {
      # In-memory path (rare)
      dd <- draws_df %>%
        dplyr::mutate(
          year_baseline = as.integer(year_baseline),
          year_changed  = as.integer(year_changed)
        ) %>%
        dplyr::left_join(avg_cover_df, by = c("site","species","year_baseline"="year")) %>%
        dplyr::rename(rc_b = mean_relative_cover) %>%
        dplyr::left_join(avg_cover_df, by = c("site","species","year_changed"="year")) %>%
        dplyr::rename(rc_c = mean_relative_cover) %>%
        dplyr::mutate(w = rowMeans(cbind(rc_b, rc_c), na.rm = TRUE)) %>%
        dplyr::filter(!is.na(w)) %>%
        dplyr::mutate(detected = as.numeric(detected))
      
      ci_df <- dd %>%
        dplyr::group_by(site, sample_size, year_baseline, year_changed, replicate, draw) %>%
        dplyr::summarise(
          cwm_draw = sum(detected * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::group_by(site, sample_size, year_baseline, year_changed, replicate) %>%
        dplyr::summarise(
          cwm_mean = mean(cwm_draw, na.rm = TRUE),     # posterior mean
          ci_lower = stats::quantile(cwm_draw, 0.025, na.rm = TRUE),
          ci_upper = stats::quantile(cwm_draw, 0.975, na.rm = TRUE),
          .groups = "drop"
        )
      
    } else if ("file" %in% names(draws_df)) {
      # Index path (stream each Parquet file)
      if (!requireNamespace("arrow", quietly = TRUE)) {
        warning("Arrow not available; CIs omitted.")
        ci_df <- NULL
      } else {
        per_file <- vector("list", nrow(draws_df))
        for (i in seq_len(nrow(draws_df))) {
          meta <- draws_df[i, , drop = FALSE]
          
          needed <- c("site","species","year_baseline","year_changed",
                      "replicate","sample_size","detected","draw",
                      "actual_sample_size","requested_sample_size")
          dt <- tryCatch(
            arrow::read_parquet(
              meta$file, as_data_frame = TRUE, col_select = tidyselect::any_of(needed)
            ),
            error = function(e) {
              warning(sprintf("Could not read parquet: %s (%s)", meta$file, conditionMessage(e)))
              NULL
            }
          )
          if (is.null(dt) || !nrow(dt)) { per_file[[i]] <- tibble::tibble(); next }
          
          # Fill keys from index if missing
          if (!"site" %in% names(dt)      && "site" %in% names(meta))      dt$site      <- meta$site
          if (!"replicate" %in% names(dt) && "replicate" %in% names(meta)) dt$replicate <- meta$replicate
          if (!"sample_size" %in% names(dt)) {
            if ("actual_sample_size" %in% names(dt))      dt$sample_size <- dt$actual_sample_size
            else if ("requested_sample_size" %in% names(dt)) dt$sample_size <- dt$requested_sample_size
            else if ("actual_sample_size" %in% names(meta)) dt$sample_size <- meta$actual_sample_size
            else if ("requested_sample_size" %in% names(meta)) dt$sample_size <- meta$requested_sample_size
            else dt$sample_size <- NA_real_
          }
          
          dt$year_baseline <- as.integer(dt$year_baseline)
          dt$year_changed  <- as.integer(dt$year_changed)
          dt$detected      <- as.numeric(dt$detected)
          
          # Join small weight table
          dtw <- dt %>%
            dplyr::left_join(avg_cover_df, by = c("site","species","year_baseline"="year")) %>%
            dplyr::rename(rc_b = mean_relative_cover) %>%
            dplyr::left_join(avg_cover_df, by = c("site","species","year_changed"="year")) %>%
            dplyr::rename(rc_c = mean_relative_cover) %>%
            dplyr::mutate(w = rowMeans(cbind(rc_b, rc_c), na.rm = TRUE)) %>%
            dplyr::filter(!is.na(w))
          
          ci_i <- dtw %>%
            dplyr::group_by(site, sample_size, year_baseline, year_changed, replicate, draw) %>%
            dplyr::summarise(
              cwm_draw = sum(detected * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::group_by(site, sample_size, year_baseline, year_changed, replicate) %>%
            dplyr::summarise(
              cwm_mean = mean(cwm_draw, na.rm = TRUE),     # posterior mean
              ci_lower = stats::quantile(cwm_draw, 0.025, na.rm = TRUE),
              ci_upper = stats::quantile(cwm_draw, 0.975, na.rm = TRUE),
              .groups = "drop"
            )
          
          per_file[[i]] <- ci_i
          rm(dt, dtw, ci_i); gc()
        }
        ci_df <- dplyr::bind_rows(per_file)
      }
    }
    
    if (!is.null(ci_df) && nrow(ci_df)) {
      community_scores <- community_scores %>%
        dplyr::left_join(ci_df,
                         by = c("site","sample_size","year_baseline","year_changed","replicate")
        )
    }
  } else {
    # No draws provided - cwm_mean will be NA, fill below
    community_scores$cwm_mean <- NA_real_
  }
  
  # ---- Canonicalize output column names ----
  community_scores <- community_scores %>%
    mutate(
      year_pair = dplyr::coalesce(.data$year_pair,
                                  paste0(.data$year_baseline, "_", .data$year_changed))
    ) %>%
    relocate(cwm_mean, .after = year_pair)
  
  # Ensure CI columns exist even if no draws
  if (!"ci_lower" %in% names(community_scores)) community_scores$ci_lower <- NA_real_
  if (!"ci_upper" %in% names(community_scores)) community_scores$ci_upper <- NA_real_
  
  # ---- Deduplicate on grouping keys ----
  # Prevent accidental duplication from upstream joins by ensuring one row per unique combination
  dedupe_keys <- c("site", "year_pair", "sample_size", "replicate", 
                   "requested_sample_size", "actual_sample_size")
  community_scores <- community_scores %>%
    dplyr::distinct(across(any_of(dedupe_keys)), .keep_all = TRUE)
  
  return(community_scores)
}
