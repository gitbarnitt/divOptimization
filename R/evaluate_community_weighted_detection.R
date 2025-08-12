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
    "detect_prob","replicate","fit_status","plot_ids"
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
    rename(site = siteID, species = taxonID)
  
  # Join cover for both years at plot level
  expanded_with_cover <- expanded_results %>%
    left_join(cover_df, by = c("site","species","plotID","year_baseline"="year")) %>%
    rename(relative_cover_baseline = relative_cover) %>%
    left_join(cover_df, by = c("site","species","plotID","year_changed"="year")) %>%
    rename(relative_cover_changed = relative_cover) %>%
    mutate(relative_cover_mean = rowMeans(cbind(relative_cover_baseline, relative_cover_changed), na.rm = TRUE)) %>%
    filter(!is.na(relative_cover_mean))
  

  # ✅ Point estimate: community-weighted mean
  # Build grouping columns; include requested/actual if available to avoid collapsing different K
  grp_cols <- c("site","year_baseline","year_changed","replicate","fit_status","sample_size")
  if (has_req) grp_cols <- c("requested_sample_size", grp_cols)
  if (has_act) grp_cols <- c(grp_cols, "actual_sample_size")
  
  community_scores <- expanded_with_cover %>%
    group_by(across(all_of(grp_cols))) %>%
    summarise(
      weighted_detection = sum(detect_prob * relative_cover_mean, na.rm = TRUE) /
        sum(relative_cover_mean, na.rm = TRUE),
      n_species = n_distinct(species),
      .groups = "drop"
    ) %>%
    mutate(year_pair = paste0(year_baseline, "_", year_changed)) %>%
    relocate(weighted_detection, .after = year_pair)
  
  # Sensible defaults if not present
  if (!"requested_sample_size" %in% names(community_scores)) {
    community_scores$requested_sample_size <- NA_real_
  }
  if (!"actual_sample_size" %in% names(community_scores)) {
    # By default, treat actual == sample_size when not explicitly tracked
    community_scores$actual_sample_size <- community_scores$sample_size
  }
  
  # ---- Optional: credible intervals from draws ----
  if (!is.null(draws_df)) {
    # If we were given an index (has `file` but no `detected`), try to read now
    if (!"detected" %in% names(draws_df) &&
        "file" %in% names(draws_df) &&
        exists("read_draws_index", mode = "function")) {
      draws_df <- read_draws_index(draws_df)
    }
    
    if ("detected" %in% names(draws_df)) {
      draws_df <- draws_df %>%
        mutate(
          year_baseline = as.integer(year_baseline),
          year_changed  = as.integer(year_changed)
        )
      
      # No plotID in draws: use mean relative cover across plots
      avg_cover_df <- relative_cover_df %>%
        rename(site = siteID, species = taxonID) %>%
        group_by(site, species, year) %>%
        summarise(mean_relative_cover = mean(relative_cover, na.rm = TRUE), .groups = "drop")
      
      draws_with_cover <- draws_df %>%
        left_join(avg_cover_df, by = c("site","species","year_baseline"="year")) %>%
        rename(rc_baseline = mean_relative_cover) %>%
        left_join(avg_cover_df, by = c("site","species","year_changed"="year")) %>%
        rename(rc_changed = mean_relative_cover) %>%
        mutate(relative_cover_mean = rowMeans(cbind(rc_baseline, rc_changed), na.rm = TRUE)) %>%
        filter(!is.na(relative_cover_mean))
      
      ci_df <- draws_with_cover %>%
        mutate(weighted = as.numeric(detected) * relative_cover_mean) %>%
        group_by(site, sample_size, year_baseline, year_changed, replicate, draw) %>%
        summarise(
          weighted_detection = sum(weighted, na.rm = TRUE) /
            sum(relative_cover_mean, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        group_by(site, sample_size, year_baseline, year_changed, replicate) %>%
        summarise(
          ci_lower = quantile(weighted_detection, 0.025, na.rm = TRUE),
          ci_upper = quantile(weighted_detection, 0.975, na.rm = TRUE),
          .groups = "drop"
        )
    }
  }
  
  # ---- Canonicalize output column names for the report/helpers ----
  community_scores <- community_scores %>%
    mutate(
      year_pair = dplyr::coalesce(.data$year_pair,
                                  paste0(.data$year_baseline, "_", .data$year_changed))
    ) %>%
    rename(cwm_mean = weighted_detection) %>%
    relocate(cwm_mean, .after = year_pair)
  
  # Ensure CI columns exist even if no draws
  if (!"ci_lower" %in% names(community_scores)) community_scores$ci_lower <- NA_real_
  if (!"ci_upper" %in% names(community_scores)) community_scores$ci_upper <- NA_real_
  
  return(community_scores)
}
