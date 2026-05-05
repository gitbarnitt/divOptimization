#' Compute Detection Probability from Pre-Computed Relative Change
#'
#' @param rel_change Matrix [draws, species] of relative change values
#' @param species_names Character vector of species names
#' @param threshold Numeric: detection threshold for relative change (default 0.20 = 20%)
#'
#' @return Numeric vector of detection probabilities per species
#'
#' @details
#' This is a simplified version of calculate_detection_probability() for use
#' when relative change has already been computed upstream (avoiding redundant
#' eps-stabilization and synthetic array construction).
#'
#' Detection probability = Pr(|rel_change| >= threshold) across posterior draws
#'
#' @keywords internal
compute_detection_from_rel_change <- function(rel_change, species_names, threshold = 0.20) {
  
  # Ensure matrix structure
  if (is.null(dim(rel_change))) {
    stop("rel_change must be a matrix [draws, species]")
  }
  
  n_species <- ncol(rel_change)
  
  if (n_species != length(species_names)) {
    stop(sprintf("Species count mismatch: rel_change has %d columns, expected %d",
                 n_species, length(species_names)))
  }
  
  # Compute detection probability per species
  detect_prob <- colMeans(abs(rel_change) >= threshold, na.rm = TRUE)
  names(detect_prob) <- species_names
  
  detect_prob
}
