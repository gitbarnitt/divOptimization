# R/compat_schema.R

canon_species <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  out <- df
  # year_pair
  if (!"year_pair" %in% names(out) && all(c("year_baseline","year_changed") %in% names(out))) {
    out <- dplyr::mutate(out, year_pair = paste0(.data$year_baseline, "_", .data$year_changed))
  }
  # sample size: fold requested/actual -> sample_size
  if ("requested_sample_size" %in% names(out) && !"sample_size" %in% names(out)) {
    out <- dplyr::rename(out, sample_size = requested_sample_size)
  }
  if ("actual_sample_size" %in% names(out) && !"sample_size" %in% names(out)) {
    out <- dplyr::rename(out, sample_size = actual_sample_size)
  }
  # mean detection name
  if ("detect_prob" %in% names(out) && !"mean_detection" %in% names(out)) {
    out <- dplyr::rename(out, mean_detection = detect_prob)
  }
  if ("cwm_mean" %in% names(out) && !"mean_detection" %in% names(out)) {
    out <- dplyr::rename(out, mean_detection = cwm_mean)
  }
  out
}

canon_community <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  out <- df
  # year_pair
  if (!"year_pair" %in% names(out) && all(c("year_baseline","year_changed") %in% names(out))) {
    out <- dplyr::mutate(out, year_pair = paste0(.data$year_baseline, "_", .data$year_changed))
  }
  # sample size: fold requested/actual -> sample_size
  if ("requested_sample_size" %in% names(out) && !"sample_size" %in% names(out)) {
    out <- dplyr::rename(out, sample_size = requested_sample_size)
  }
  if ("actual_sample_size" %in% names(out) && !"sample_size" %in% names(out)) {
    out <- dplyr::rename(out, sample_size = actual_sample_size)
  }
  # community mean: fold cwm_mean -> weighted_detection
  if ("cwm_mean" %in% names(out) && !"weighted_detection" %in% names(out)) {
    out <- dplyr::rename(out, weighted_detection = cwm_mean)
  }
  out
}
