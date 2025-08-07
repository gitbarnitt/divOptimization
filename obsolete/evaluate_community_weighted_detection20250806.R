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
  
  # Check required columns
  required_cols <- c("site", "sample_size", "species", "year_baseline", "year_changed",
                     "detect_prob", "replicate", "fit_status", "plot_ids")  # added "plot_ids" 20250806
  if (!all(required_cols %in% colnames(sensitivity_results))) {
    stop("❌ sensitivity_results is missing required columns.")
  }
  
  if (!all(c("siteID", "plotID", "year", "taxonID", "relative_cover") %in% colnames(relative_cover_df))) {
    stop("❌ relative_cover_df is missing required columns.")
  }
  
  # Combine year_baseline and year_changed into year_pair
  results_with_pairs <- sensitivity_results %>%
    mutate(year_pair = paste0(year_baseline, "_", year_changed))
  
  # # Join cover data (rename for merge)            #this block replaced 20250806
  # cover_renamed <- relative_cover_df %>%
  #   rename(site = siteID, species = taxonID)
  # 
  # # Join to attach relative cover per species-site-year
  # merged <- results_with_pairs %>%
  #   left_join(cover_renamed, by = c("site", "species"))
  
  # Rename cover columns for join
  cover_renamed <- relative_cover_df %>%
    rename(site = siteID, species = taxonID)
  
  # Attach plot_ids to results
  results_with_pairs <- results_with_pairs %>%
    dplyr::filter(!is.na(plot_ids))  # Just in case
  
  # Use rowwise to subset cover_df for each replicate
  expanded_results <- results_with_pairs %>%
    rowwise() %>%
    mutate(
      rel_cover = list(
        cover_renamed %>%
          filter(site == site,
                 species == species,  # added species filter 20250806
                 year %in% c(as.integer(year_baseline), as.integer(year_changed)),   # explicit coercion
                 plotID %in% plot_ids)
                   
                 #year %in% c(year_baseline, year_changed))
      )
    ) %>%
      unnest(rel_cover) %>%  # removed `names_sep` to preserve original column names 20250806
      #unnest(rel_cover, names_sep = "_rel")
      ungroup()  #added 20250806
      
  
  # Select only necessary columns for aggregation
  expanded_results <- expanded_results %>%
    select(site, sample_size, year_pair, replicate, fit_status, species,
           detect_prob, relative_cover, plotID, year)
  
  # Aggregate to compute weighted detection
  #weighted_scores <- merged %>% #updated 20250806
  weighted_scores <- expanded_results %>%
    group_by(site, sample_size, year_pair, replicate, fit_status) %>%
    summarise(
      weighted_detection = sum(detect_prob * relative_cover, na.rm = TRUE) / 
        sum(relative_cover, na.rm = TRUE),
      n_species = dplyr::n(),
      .groups = "drop"
    )
   
   #   weighted_detection = sum(detect_prob * relative_cover, na.rm = TRUE) / removed 20250806
    #     sum(relative_cover, na.rm = TRUE),
    #   n_species = dplyr::n(),
    #   .groups = "drop"
    # )
  
  return(weighted_scores)
}
