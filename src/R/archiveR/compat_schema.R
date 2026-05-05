# R/compat_schema.R
# SIMPLIFIED: All functions now produce canonical names from the start
# This just handles optional year_pair derivation if needed

canon_species <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  out <- df
  # Derive year_pair if missing but components exist
  if (!"year_pair" %in% names(out) && all(c("year_baseline","year_changed") %in% names(out))) {
    out <- dplyr::mutate(out, year_pair = paste0(.data$year_baseline, "_", .data$year_changed))
  }
  out
}

canon_community <- function(df) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  out <- df
  # Derive year_pair if missing but components exist
  if (!"year_pair" %in% names(out) && all(c("year_baseline","year_changed") %in% names(out))) {
    out <- dplyr::mutate(out, year_pair = paste0(.data$year_baseline, "_", .data$year_changed))
  }
  out
}
