estimate_detection_prob <- function(models) {
  purrr::map_dfr(models, function(mod) {
    means <- apply(mod$chains$sgibbs, 2, mean)
    lowers <- apply(mod$chains$sgibbs, 2, quantile, probs = 0.025)
    uppers <- apply(mod$chains$sgibbs, 2, quantile, probs = 0.975)
    tibble(
      species = names(means),
      mean = means,
      lower = lowers,
      upper = uppers
    )
  }, .id = "site")
}