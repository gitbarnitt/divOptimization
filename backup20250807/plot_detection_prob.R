plot_detection_prob <- function(detection_summary, output_file = "outputs/detection_plot.png") {
  if (is.null(detection_summary) || nrow(detection_summary) == 0) {
    warning("No detection summary data to plot.")
    return(NULL)
  }
  
  library(ggplot2)
  library(dplyr)
  
  detection_summary <- detection_summary %>%
    mutate(species = factor(species),
           siteID = factor(siteID),
           direction = factor(direction))
  
  p <- ggplot(detection_summary, aes(x = siteID, y = species, fill = detection_prob)) +
    geom_tile(color = "white") +
    facet_wrap(~ direction) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5,
                         limits = c(0, 1), name = "Detection\nProbability") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = "Detection Probability by Species and Site",
         x = "Site",
         y = "Species")
  
  ggsave(output_file, p, width = 10, height = 8)
  return(output_file)
}
