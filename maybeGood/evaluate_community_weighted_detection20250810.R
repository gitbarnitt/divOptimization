#' Evaluate Community-Weighted Detection Score
#'
#' Computes the community-weighted mean detection probability for each site, sample size, replicate, and year pair,
#' based on the relative cover of each species.
#'
#' @param sensitivity_results Data frame returned by run_sample_size_sensitivity()
#' @param relative_cover_df Data frame of relative cover (output of compute_relative_cover())
#'
#' @return A tibble with columns: site, sample_size, year_pair, replicate, weighted_detection, fit_status
#'
#' @export
evaluate_community_weighted_detection <- function(
    sensitivity_results,
    relative_cover_df,
    draws_df = NULL
) {
  library(dplyr)
  library(tidyr)
  
  # ✅ Check required input columns
  required_summary_cols <- c("site", "sample_size", "species", "year_baseline", "year_changed",
                             "detect_prob", "replicate", "fit_status", "plot_ids")
  if (!all(required_summary_cols %in% names(sensitivity_results))) {
    stop("❌ sensitivity_results is missing required columns.")
  }
  
  required_cover_cols <- c("siteID", "plotID", "year", "taxonID", "relative_cover")
  if (!all(required_cover_cols %in% names(relative_cover_df))) {
    stop("❌ relative_cover_df is missing required columns.")
  }
  
  # ✅ Expand plot IDs
  expanded_results <- sensitivity_results %>%
    mutate(year_baseline = as.integer(year_baseline),
           year_changed  = as.integer(year_changed)) %>%
    unnest(plot_ids, names_repair = "minimal") %>%
    rename(plotID = plot_ids)
  
  # ✅ Prepare cover data
  cover_df <- relative_cover_df %>%
    rename(site = siteID, species = taxonID)
  
  # ✅ Join baseline and changed years
  expanded_with_cover <- expanded_results %>%
    left_join(cover_df, by = c("site", "species", "plotID", "year_baseline" = "year")) %>%
    rename(relative_cover_baseline = relative_cover) %>%
    left_join(cover_df, by = c("site", "species", "plotID", "year_changed" = "year")) %>%
    rename(relative_cover_changed = relative_cover) %>%
    mutate(relative_cover_mean = rowMeans(cbind(relative_cover_baseline, relative_cover_changed), na.rm = TRUE)) %>%
    filter(!is.na(relative_cover_mean))
  
  # ✅ Point estimate: community-weighted mean
  community_scores <- expanded_with_cover %>%
    group_by(site, sample_size, year_baseline, year_changed, replicate, fit_status) %>%
    summarise(
      weighted_detection = sum(detect_prob * relative_cover_mean, na.rm = TRUE) /
        sum(relative_cover_mean, na.rm = TRUE),
      n_species = n_distinct(species),
      .groups = "drop"
    ) %>%
    mutate(year_pair = paste0(year_baseline, "_", year_changed)) %>%
    relocate(weighted_detection, .after = year_pair)
  
  # ✅ Optional: Add credible intervals from posterior draws
  if (!is.null(draws_df)) {
    # Convert year columns to integer to match cover
    draws_df <- draws_df %>%
      mutate(
        year_baseline = as.integer(year_baseline),
        year_changed  = as.integer(year_changed)
      )
    
    # Posterior draws don't include plotID → use mean cover across plots
    avg_cover_df <- relative_cover_df %>%
      rename(site = siteID, species = taxonID) %>%
      group_by(site, species, year) %>%
      summarise(
        mean_relative_cover = mean(relative_cover, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Join relative cover to draws
    draws_with_cover <- draws_df %>%
      left_join(avg_cover_df, by = c("site", "species", "year_baseline" = "year")) %>%
      rename(rc_baseline = mean_relative_cover) %>%
      left_join(avg_cover_df, by = c("site", "species", "year_changed" = "year")) %>%
      rename(rc_changed = mean_relative_cover) %>%
      mutate(relative_cover_mean = rowMeans(cbind(rc_baseline, rc_changed), na.rm = TRUE)) %>%
      filter(!is.na(relative_cover_mean))
    
    # Compute weighted detection per draw
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
        ci_lower = quantile(weighted_detection, 0.025),
        ci_upper = quantile(weighted_detection, 0.975),
        .groups = "drop"
      )
    
    # Join back to community_scores
    community_scores <- community_scores %>%
      left_join(ci_df, by = c("site", "sample_size", "year_baseline", "year_changed", "replicate"))
  }
  
  community_scores <- community_scores %>%
    dplyr::mutate(
      year_pair = dplyr::coalesce(
        .data$year_pair,
        paste0(.data$year_baseline, "_", .data$year_changed)
      )
    ) %>%
    # Make sure the mean column has a stable name for downstream helpers/Rmd
    dplyr::rename(cwm_mean = weighted_detection) %>%
    dplyr::relocate(cwm_mean, .after = year_pair)
}
