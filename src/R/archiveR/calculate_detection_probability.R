calculate_detection_probability <- function(
    posterior_preds,
    year_pair,
    site_id,
    sample_size,
    threshold = 0.20  # relative change threshold (20%)
) {
  # Ensure array structure: [draws, 2, species]
  if (length(dim(posterior_preds)) != 3 || dim(posterior_preds)[2] != 2) {
    stop("posterior_preds must be a 3D array with second dim = 2 (baseline, changed).")
  }
  
  n_species     <- dim(posterior_preds)[3]
  species_names <- dimnames(posterior_preds)[[3]]
  n_draws       <- dim(posterior_preds)[1]
  
  baseline <- posterior_preds[, 1, ]
  changed  <- posterior_preds[, 2, ]
  
  # Compute relative change with stabilization for percent cover (0-100 scale)
  eps <- 0.01  # percent cover units
  rel_change <- (changed - baseline) / pmax(abs(baseline), eps)
  
  # Ensure rel_change is a matrix even for 1 species
  if (n_species == 1) {
    rel_change <- matrix(rel_change, ncol = 1)
    colnames(rel_change) <- species_names
  }
  
  # Compute species-level summaries on relative change
  detect_prob      <- colMeans(abs(rel_change) >= threshold)
  mean_rel_change  <- colMeans(rel_change)
  ci_bounds        <- apply(rel_change, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  
  summary <- tibble::tibble(
    site              = site_id,
    sample_size       = sample_size,
    species           = species_names,
    year_baseline     = year_pair[1],
    year_changed      = year_pair[2],
    mean_detection    = detect_prob,
    mean_rel_change   = mean_rel_change,
    ci_lower          = ci_bounds[1, ],
    ci_upper          = ci_bounds[2, ]
  )
  
  # Also return detection draws per species (based on relative change)
  draws <- as.data.frame(abs(rel_change) >= threshold) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(draw = 1:n_draws, .before = 1) %>%
    tidyr::pivot_longer(
      cols = -draw,
      names_to = "species",
      values_to = "detected"
    ) %>%
    dplyr::mutate(
      site          = site_id,
      sample_size   = sample_size,
      year_baseline = year_pair[1],
      year_changed  = year_pair[2]
    ) %>%
    dplyr::relocate(site:sample_size, .before = draw)
  
  return(list(summary = summary, draws = draws))
}
