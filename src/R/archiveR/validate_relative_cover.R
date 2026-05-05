# R/validate_relative_cover.R
validate_relative_cover <- function(df) {
  # Check required columns exist
  required <- c("siteID", "plotID", "year", "taxonID", "relative_cover")
  missing <- setdiff(required, names(df))
  
  if (length(missing) > 0) {
    stop(sprintf(
      "validate_relative_cover: Missing required columns: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  
  # Check for reasonable values
  if (any(df$relative_cover < 0 | df$relative_cover > 1, na.rm = TRUE)) {
    warning("validate_relative_cover: relative_cover values outside [0,1] range")
  }
  
  message(sprintf(
    "[validate_relative_cover] ✓ %d rows, %d taxa, %d sites",
    nrow(df),
    length(unique(df$taxonID)),
    length(unique(df$siteID))
  ))
  
  df
}
