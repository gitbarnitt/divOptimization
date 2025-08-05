run_sample_size_sensitivity <- function(
    fit_result,               # output of fit_gjam_model_test()
    sample_sizes = c(5, 10, 15, 20, 25),
    n_replicates = 3,
    seed = 123
) {
  set.seed(seed)
  results <- list()
  
  xdata <- fit_result$fit$xdata
  site_id <- fit_result$site
  all_plots <- unique(xdata$plotID)
  total_plots <- length(all_plots)
  
  # Filter sample_sizes to those that can be supported by the data
  valid_sample_sizes <- sample_sizes[sample_sizes <= total_plots]
  
  for (size in valid_sample_sizes) {
    for (rep in seq_len(n_replicates)) {
      message(glue::glue("🔁 {site_id}: {size} plots, replicate {rep}"))
      
      sampled_plots <- sample(all_plots, size)
      xdata_subset <- dplyr::filter(xdata, plotID %in% sampled_plots)
      
      # Simulate predictions for all year pairs
      posterior_list <- loop_simulate_changes_with_index(     #CHANGED 20250805: was posterior_list <- loop_simulate_changes
        fit = fit_result$fit,
        plot_index = which(xdata$plotID %in% sampled_plots)
      )
      
      # Calculate detection probability
      summary_list <- purrr::imap(posterior_list, ~ 
                                    calculate_detection_probability(
                                      posterior_preds = .x,
                                      year_pair       = strsplit(.y, "_")[[1]],
                                      site_id         = site_id,
                                      sample_size     = size
                                    )
      )
      
      # Combine and label
      summary_df <- dplyr::bind_rows(summary_list) %>%
        dplyr::mutate(
          replicate  = rep,
          fit_status = "ok"
        )
      
      results[[paste0("size_", size, "_rep_", rep)]] <- summary_df
    }
  }
  
  final_result <- dplyr::bind_rows(results)
  return(final_result)
}
