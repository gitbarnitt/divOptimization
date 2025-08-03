calculate_detection_probability <- function(
    posterior_array,   # [draws, 2, species] from simulate_change()
    site_id,           # e.g., "JERC"
    sample_size,       # number of plots used
    year_pair = c("2015", "2016")  # comparison years: baseline vs change
) {
  # Dimensions
  n_draws <- dim(posterior_array)[1]
  species_names <- dimnames(posterior_array)[[3]]
  
  results <- lapply(species_names, function(sp) {
    baseline <- posterior_array[, 1, sp]
    changed  <- posterior_array[, 2, sp]
    difference <- changed - baseline
    
    # Posterior probability of detecting a change (i.e., effect direction consistently non-zero)
    detect_prob <- mean(difference > 0 | difference < 0)  # Conservative two-tailed
    
    # Summary statistics
    mean_diff <- mean(difference)
    ci_lower  <- quantile(difference, 0.025)
    ci_upper  <- quantile(difference, 0.975)
    
    tibble::tibble(
      site          = site_id,
      sample_size   = sample_size,
      species       = sp,
      year_baseline = year_pair[1],
      year_changed  = year_pair[2],
      detect_prob   = detect_prob,
      mean_diff     = mean_diff,
      ci_lower      = ci_lower,
      ci_upper      = ci_upper
    )
  })
  
  dplyr::bind_rows(results)
}
