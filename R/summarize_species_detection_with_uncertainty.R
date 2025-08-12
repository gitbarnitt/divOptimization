summarize_species_detection_with_uncertainty <- function(
    summary_df,  # results$summary
    draws_df     # results$draws OR an index tibble from read_draws_index()
) {
  library(dplyr)
  library(tidyr)
  
  # --- Species-level point summaries (from summary_df) ---
  species_summary <- summary_df %>%
    mutate(
      year_baseline = as.integer(year_baseline),
      year_changed  = as.integer(year_changed),
      year_pair     = dplyr::coalesce(
        .data$year_pair,
        paste0(.data$year_baseline, "_", .data$year_changed)
      )
    ) %>%
    # canonical mean column name expected by the report/helpers
    dplyr::rename(mean_detection = detect_prob) %>%
    dplyr::relocate(mean_detection, .after = year_pair)
  
  # --- If no draws provided, return with NA CIs (safe no-op) ---
  if (is.null(draws_df) || !is.data.frame(draws_df) || !nrow(draws_df)) {
    return(species_summary %>%
             mutate(ci_lower = NA_real_, ci_upper = NA_real_))
  }
  
  # --- If we were given an INDEX (has `file` but no `detected`), load the parquet now ---
  if (!"detected" %in% names(draws_df) &&
      "file" %in% names(draws_df) &&
      exists("read_draws_index", mode = "function")) {
    draws_df <- read_draws_index(draws_df)
  }
  
  # --- Still no `detected`? Cannot compute CIs: return NAs rather than error ---
  if (!"detected" %in% names(draws_df)) {
    return(species_summary %>%
             mutate(ci_lower = NA_real_, ci_upper = NA_real_))
  }
  
  # --- Compute CIs from posterior draws ---
  ci_draws <- draws_df %>%
    mutate(
      year_baseline = as.integer(year_baseline),
      year_changed  = as.integer(year_changed)
    ) %>%
    group_by(site, sample_size, species, year_baseline, year_changed, replicate) %>%
    summarise(
      ci_lower = quantile(detected, 0.025, na.rm = TRUE),
      ci_upper = quantile(detected, 0.975, na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    mutate(year_pair = paste0(year_baseline, "_", year_changed))
  
  # --- Join back to the point summaries ---
  result <- species_summary %>%
    left_join(
      ci_draws,
      by = c("site", "sample_size", "species",
             "year_baseline", "year_changed", "replicate", "year_pair")
    )
  
  # ensure CI columns exist even if join is empty
  if (!"ci_lower" %in% names(result)) result$ci_lower <- NA_real_
  if (!"ci_upper" %in% names(result)) result$ci_upper <- NA_real_
  
  return(result)
}
