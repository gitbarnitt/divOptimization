summarize_species_detection_with_uncertainty <- function(
    summary_df,  # results$summary
    draws_df     # results$draws
) {
  library(dplyr)
  library(tidyr)
  
  # Summary from mean posterior estimates (already in summary_df)
  species_summary <- summary_df %>%
    mutate(year_pair = paste0(year_baseline, "_", year_changed)) %>%
    relocate(detect_prob, .after = year_pair)
  
  # Summary from posterior draws
  ci_draws <- draws_df %>%
    group_by(site, sample_size, species, year_baseline, year_changed, replicate) %>%
    summarise(
      ci_lower = quantile(detected, 0.025, na.rm = TRUE),
      ci_upper = quantile(detected, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(year_pair = paste0(year_baseline, "_", year_changed))
  
  # Join and return
  result <- species_summary %>%
    left_join(ci_draws,
              by = c("site", "sample_size", "species",
                     "year_baseline", "year_changed", "replicate", "year_pair"))
  
  return(result)
}
