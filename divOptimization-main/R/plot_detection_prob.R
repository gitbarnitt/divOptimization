plot_detection_prob <- function(df) {
  ggplot(df, aes(x = species, y = mean, ymin = lower, ymax = upper)) +
    geom_pointrange() +
    facet_wrap(~site, scales = "free") +
    theme_bw() +
    labs(title = "Detection Probability Estimates", y = "Probability", x = "Species")
}