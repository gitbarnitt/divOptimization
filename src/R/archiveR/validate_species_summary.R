# R/validate_species_summary.R
validate_species_summary <- function(df) {
  # Check required columns exist
  required <- c("site", "species", "sample_size", "year_baseline", "year_changed", 
                "mean_detection", "replicate")
  missing <- setdiff(required, names(df))
  
  if (length(missing) > 0) {
    stop(sprintf(
      "validate_species_summary: Missing required columns: %s",
      paste(missing, collapse = ", ")
    ), call. = FALSE)
  }
  
  # Check for reasonable values
  if (any(df$mean_detection < 0 | df$mean_detection > 1, na.rm = TRUE)) {
    warning("validate_species_summary: mean_detection values outside [0,1] range")
  }
  
  if (any(df$sample_size < 1, na.rm = TRUE)) {
    warning("validate_species_summary: sample_size < 1 detected")
  }
  
  message(sprintf(
    "[validate_species_summary] ✓ %d rows, %d species, %d sites",
    nrow(df),
    length(unique(df$species)),
    length(unique(df$site))
  ))
  
  df
}
