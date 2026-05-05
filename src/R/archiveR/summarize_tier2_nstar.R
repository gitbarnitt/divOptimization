#' Summarize N* across Tier 2A replicates
#'
#' @param replicate_results list of run_tier2_replicate() outputs (each with $summary and $n_star_details)
#'
#' @return tibble with:
#'   - site
#'   - n_star_median, n_star_q05/q25/q75/q95 (distribution across replicates)
#'   - B (total replicates)
#'   - fit_fail_rate (fraction of replicates that failed)
#'   - mean_fit_time_sec
#'   - diagnostics: n_plots_unique_mean, n_unique_hashes
summarize_tier2_nstar <- function(replicate_results) {
  
  # Extract summary metadata from each replicate
  summary_list <- lapply(replicate_results, function(x) x$summary)
  summary_df <- dplyr::bind_rows(summary_list)
  
  # Extract detailed N* results and compute replicate-level median N*
  details_list <- lapply(replicate_results, function(x) {
    if (nrow(x$n_star_details) == 0) return(NULL)  # Skip failures
    
    # For each replicate, compute median N* across species/year-pairs
    tibble::tibble(
      site = unique(x$summary$site),
      replicate_id = unique(x$summary$replicate_id),
      n_star_replicate_median = median(x$n_star_details$n_star_median, na.rm = TRUE)
    )
  })
  
  details_df <- dplyr::bind_rows(details_list)
  
  # Join with summary metadata
  full_df <- summary_df %>%
    left_join(details_df, by = c("site", "replicate_id"))
  
  # Summarize across replicates
  full_df %>%
    dplyr::group_by(site) %>%
    dplyr::summarise(
      # N* distribution (computed from successful refits)
      n_star_median = median(n_star_replicate_median, na.rm = TRUE),
      n_star_q05 = quantile(n_star_replicate_median, 0.05, na.rm = TRUE),
      n_star_q25 = quantile(n_star_replicate_median, 0.25, na.rm = TRUE),
      n_star_q75 = quantile(n_star_replicate_median, 0.75, na.rm = TRUE),
      n_star_q95 = quantile(n_star_replicate_median, 0.95, na.rm = TRUE),
      
      # Refit metadata
      B = dplyr::n(),
      n_success = sum(fit_status == "ok"),
      n_fail = sum(fit_status == "fail"),
      fit_fail_rate = mean(fit_status == "fail"),
      
      # Diagnostics
      mean_fit_time_sec = mean(fit_time_sec[fit_status == "ok"], na.rm = TRUE),
      n_plots_unique_mean = mean(n_plots_unique[fit_status == "ok"], na.rm = TRUE),
      n_unique_hashes = dplyr::n_distinct(plot_ids_hash[fit_status == "ok"]),
      sample_frac = dplyr::first(sample_frac),
      
      .groups = "drop"
    )
}
