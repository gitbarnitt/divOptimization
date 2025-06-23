generate_plot_subsamples <- function(site_data, sizes) {
  lapply(sizes, function(n) {
    dplyr::sample_n(site_data, size = min(n, nrow(site_data)))
  })
}