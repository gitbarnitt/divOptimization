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
evaluate_community_weighted_detection <- function(sensitivity_results, relative_cover_df) {
  library(dplyr)
  library(tidyr)
  
  # Validate inputs
  required_cols <- c("site", "sample_size", "species", "year_baseline", "year_changed",
                     "detect_prob", "replicate", "fit_status", "plot_ids")
  if (!all(required_cols %in% names(sensitivity_results))) {
    stop("❌ sensitivity_results is missing required columns.")
  }
  
  if (!all(c("siteID", "plotID", "year", "taxonID", "relative_cover") %in% names(relative_cover_df))) {
    stop("❌ relative_cover_df is missing required columns.")
  }
  
  # Expand plot_ids
  expanded_results <- sensitivity_results %>%
    mutate(year_baseline = as.integer(year_baseline),
           year_changed  = as.integer(year_changed)) %>%
    unnest(plot_ids, names_repair = "minimal") %>%
    rename(plotID = plot_ids)
  
  # Prepare relative cover data
  cover_df <- relative_cover_df %>%
    rename(site = siteID, species = taxonID)
  
  # Join baseline and changed year cover
  expanded_with_cover <- expanded_results %>%
    left_join(cover_df, by = c("site", "species", "plotID", "year_baseline" = "year")) %>%
    rename(relative_cover_baseline = relative_cover) %>%
    left_join(cover_df, by = c("site", "species", "plotID", "year_changed" = "year")) %>%
    rename(relative_cover_changed = relative_cover) %>%
    mutate(relative_cover_mean = rowMeans(cbind(relative_cover_baseline, relative_cover_changed), na.rm = TRUE)) %>%
    filter(!is.na(relative_cover_mean))  # ✅ Exclude species with no data
  
  # Summarise to community-level detection
  community_scores <- expanded_with_cover %>%
    group_by(site, sample_size, year_baseline, year_changed, replicate, fit_status) %>%
    summarise(
      weighted_detection = sum(detect_prob * relative_cover_mean, na.rm = TRUE) /
        sum(relative_cover_mean, na.rm = TRUE),
      n_species = n_distinct(species),  # ✅ Count unique species only
      .groups = "drop"
    ) %>%
    mutate(year_pair = paste0(year_baseline, "_", year_changed)) %>%
    relocate(weighted_detection, .after = year_pair)
  
  return(community_scores)
}
