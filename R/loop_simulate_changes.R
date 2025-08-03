loop_simulate_changes <- function(fit, plot_index = 1) {
  # Extract unique years in order
  years <- sort(unique(as.integer(levels(fit$xdata$year))))
  
  # Generate all consecutive year pairs
  year_pairs <- purrr::map2_chr(
    years[-length(years)],
    years[-1],
    ~ paste0(.x, "_", .y)
  )
  
  # Named list of posterior prediction arrays
  results <- purrr::map(year_pairs, function(pair_label) {
    year_split <- strsplit(pair_label, "_")[[1]]
    year_pair <- as.character(year_split)  # Ensure they match factor levels
    
    simulate_change(
      fit = fit,
      change_year = year_pair,
      plot_index = plot_index
    )
  })
  
  names(results) <- year_pairs
  return(results)
}
