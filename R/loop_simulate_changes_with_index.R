loop_simulate_changes_with_index <- function(fit, plot_index) {
  xdata_subset <- droplevels(fit$xdata[plot_index, ])
  
  # Ensure factors are aligned
  xdata_subset$year <- factor(xdata_subset$year, levels = levels(fit$xdata$year))
  xdata_subset$nlcdClass <- factor(xdata_subset$nlcdClass, levels = levels(fit$xdata$nlcdClass))
  
  # Get year pairs
  years <- sort(unique(as.integer(levels(fit$xdata$year))))
  year_pairs <- purrr::map2_chr(
    years[-length(years)],
    years[-1],
    ~ paste0(.x, "_", .y)
  )
  
  results <- purrr::map(year_pairs, function(pair_label) {
    simulate_change(
      fit = fit,
      change_year = strsplit(pair_label, "_")[[1]],
      xdata_subset = xdata_subset
    )
  })
  
  names(results) <- year_pairs
  return(results)
}
