loop_simulate_changes <- function(fit, n_plots = 5) {
  # Extract unique years in order
  years <- sort(unique(as.integer(levels(fit$xdata$year))))
  
  # Generate all consecutive year pairs
  year_pairs <- purrr::map2_chr(
    years[-length(years)],
    years[-1],
    ~ paste0(.x, "_", .y)
  )
  
  # Sample N plots (same subset used for all year comparisons)
  set.seed(42)  # For reproducibility
  xdata_subset <- droplevels(fit$xdata[sample(nrow(fit$xdata), n_plots), ])
  
  # Ensure factors have correct levels
  xdata_subset$year <- factor(xdata_subset$year, levels = levels(fit$xdata$year))
  xdata_subset$nlcdClass <- factor(xdata_subset$nlcdClass, levels = levels(fit$xdata$nlcdClass))
  
  # Run simulate_change for each year pair
  results <- purrr::map(year_pairs, function(pair_label) {
    year_split <- strsplit(pair_label, "_")[[1]]
    year_pair <- as.character(year_split)
    
    simulate_change(
      fit = fit,
      change_year = year_pair,
      xdata_subset = xdata_subset
    )
  })
  
  names(results) <- year_pairs
  return(results)  # named list of posterior prediction arrays
}
