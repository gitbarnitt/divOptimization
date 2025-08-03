calculate_detection_probability <- function(
    posterior_preds,
    year_pair,
    site_id,
    sample_size,
    threshold = 0
) {
  # Ensure array structure: [draws, 2, species]
  if (length(dim(posterior_preds)) != 3 || dim(posterior_preds)[2] != 2) {
    stop("posterior_preds must be a 3D array with second dim = 2 (baseline, changed).")
  }
  
  n_species <- dim(posterior_preds)[3]
  species_names <- dimnames(posterior_preds)[[3]]
  
  baseline <- posterior_preds[, 1, ]
  changed  <- posterior_preds[, 2, ]
  
  diffs <- changed - baseline
  
  # Ensure diffs is a matrix even for 1 species
  if (n_species == 1) {
    diffs <- matrix(diffs, ncol = 1)
    colnames(diffs) <- species_names
  }
  
  # Compute statistics
  detect_prob <- colMeans(diffs > threshold)
  mean_diff   <- colMeans(diffs)
  ci_bounds   <- apply(diffs, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Assemble result
  tibble::tibble(
    site          = site_id,
    sample_size   = sample_size,
    species       = species_names,
    year_baseline = year_pair[1],
    year_changed  = year_pair[2],
    detect_prob   = detect_prob,
    mean_diff     = mean_diff,
    ci_lower      = ci_bounds[1, ],
    ci_upper      = ci_bounds[2, ]
  )
}
