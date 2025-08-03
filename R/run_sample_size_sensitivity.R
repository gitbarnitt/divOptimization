run_sample_size_sensitivity <- function(
    full_site_data,             # Full tibble of all years, all plots for one site
    site_id,                    # e.g. "JERC"
    sample_sizes = c(5, 10, 15),# Vector of plot counts to test
    n_replicates = 3,           # Repeats per sample size
    seed = 123
) {
  set.seed(seed)
  results <- list()
  
  all_plots <- unique(full_site_data$plotID)
  
  for (size in sample_sizes) {
    for (rep in seq_len(n_replicates)) {
      message(glue::glue("🔁 {site_id}: {size} plots, replicate {rep}"))
      
      sampled_plots <- sample(all_plots, size)
      site_data_subset <- full_site_data %>%
        dplyr::filter(plotID %in% sampled_plots)
      
      # Fit model
      fit_result <- fit_gjam_model_test(site_data_subset)
      
      # Skip if fit failed
      if (is.null(fit_result)) next
      
      # Simulate predictions for all year pairs
      posterior_list <- loop_simulate_changes(fit_result$fit)
      
      # Calculate detection probability for each year-pair
      summary_list <- purrr::imap(posterior_list, ~ 
                                    calculate_detection_probability(
                                      posterior_array = .x,
                                      year_pair = strsplit(.y, "_")[[1]],
                                      site_id = fit_result$site,
                                      sample_size = size
                                    )
      )
      
      # Combine and tag replicate
      summary_df <- dplyr::bind_rows(summary_list) %>%
        dplyr::mutate(replicate = rep)
      
      results[[paste0("size_", size, "_rep_", rep)]] <- summary_df
    }
  }
  
  final_result <- dplyr::bind_rows(results)
  return(final_result)
}
