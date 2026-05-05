read_draws_index <- function(draws_idx,
                             year_pair = NULL,
                             sample_size = NULL) {
  idx <- draws_idx
  if (!is.null(year_pair))   idx <- dplyr::filter(idx, year_pair %in% !!year_pair)
  if (!is.null(sample_size)) idx <- dplyr::filter(idx, actual_sample_size %in% !!sample_size)
  if (nrow(idx) == 0) return(tibble::tibble())
  parts <- lapply(idx$file, arrow::read_parquet)
  dplyr::bind_rows(parts)
}