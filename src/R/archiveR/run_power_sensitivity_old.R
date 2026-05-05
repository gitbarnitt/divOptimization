#' Run power analysis across sample sizes
#' 
#' @param sim_data Output from simulate_perturbed_dataset()
#' @param n_grid Integer vector: sample sizes to test (e.g., 2:15)
#' @param power_reps Integer: Monte Carlo replicates per sample size
#' @param site_id Character: site identifier
#' @param threshold Numeric: detection threshold for relative change (default 0.20 = 20%)
#' @param power_threshold Numeric: threshold for defining N* (default 0.80 = 80% power)
#' @param decision_rule Character: "posterior_prob" (average detection probability) or 
#'                                   "binary_detect" (0/1 decision per rep)
#' @param posterior_cutoff Numeric: if decision_rule="binary_detect", threshold for calling detected
#' @param seed Integer: random seed
#' 
#' @details
#' Power calculation:
#'   - For each sample size K and each Monte Carlo replicate:
#'     1. Sample K plots (same plots for baseline and perturbed)
#'     2. Average predictions across K plots for each posterior draw
#'     3. Compute detection using calculate_detection_probability()
#'   
#'   - decision_rule="posterior_prob" (default):
#'     * Extracts mean_detection (posterior prob of exceeding threshold)
#'     * Power = mean(mean_detection across MC reps)
#'     * Interpretation: Expected posterior probability of detection
#'   
#'   - decision_rule="binary_detect":
#'     * Converts mean_detection to 0/1 using posterior_cutoff
#'     * Power = mean(detected 0/1 across MC reps)
#'     * Interpretation: Frequentist power (probability of a "positive" result)
#' 
#' @return List with:
#'   - power_curve: tibble(site, sample_size, species, power, decision_rule)
#'   - n_star: tibble(site, species, n_star, power_achieved, power_threshold)
run_power_sensitivity <- function(
    sim_data,
    n_grid,
    power_reps,
    site_id,
    threshold = 0.20,
    power_threshold = 0.80,
    decision_rule = c("posterior_prob", "binary_detect"),
    posterior_cutoff = 0.80,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  decision_rule <- match.arg(decision_rule)
  
  y_base <- sim_data$y_base_sim  # [draws, plots, species]
  y_pert <- sim_data$y_pert_sim
  plot_ids <- sim_data$plot_ids
  n_plots_total <- length(plot_ids)
  n_species <- dim(y_base)[3]
  species_names <- dimnames(y_base)[[3]]
  
  year_pair <- c(sim_data$meta$year_baseline, sim_data$meta$year_perturbed)
  
  message(sprintf(
    "[TierP0] Power analysis: %d sample sizes × %d reps × %d species (rule=%s)",
    length(n_grid), power_reps, n_species, decision_rule
  ))
  
  results_list <- list()
  
  for (K in n_grid) {
    if (K > n_plots_total) {
      warning(sprintf("Skipping K=%d (exceeds available plots=%d)", K, n_plots_total))
      next
    }
    
    # Store detection metric per replicate × species
    detection_matrix <- matrix(
      NA_real_,
      nrow = power_reps,
      ncol = n_species,
      dimnames = list(NULL, species_names)
    )
    
    for (rep in 1:power_reps) {
      # Sample K plots (same plots for baseline and perturbed)
      sampled_idx <- sample.int(n_plots_total, size = K, replace = FALSE)
      
      # Average across the K sampled plots for each posterior draw
      base_avg <- apply(y_base[, sampled_idx, , drop = FALSE], c(1, 3), mean)  # [draws, species]
      pert_avg <- apply(y_pert[, sampled_idx, , drop = FALSE], c(1, 3), mean)  # [draws, species]
      
      # Combine into [draws, 2, species] format for calculate_detection_probability
      preds <- array(
        NA_real_,
        dim = c(nrow(base_avg), 2, n_species),
        dimnames = list(NULL, c("baseline", "changed"), species_names)
      )
      preds[, 1, ] <- base_avg
      preds[, 2, ] <- pert_avg
      
      # Compute detection for this replicate
      det_result <- calculate_detection_probability(
        posterior_preds = preds,
        year_pair = year_pair,
        site_id = site_id,
        sample_size = K,
        threshold = threshold
      )
      
      # Extract detection metric based on decision rule
      if (decision_rule == "posterior_prob") {
        # Use posterior probability of detection directly
        detection_matrix[rep, ] <- det_result$summary$mean_detection
      } else {
        # Convert to binary decision (detected = 1 if posterior prob >= cutoff)
        detection_matrix[rep, ] <- as.integer(det_result$summary$mean_detection >= posterior_cutoff)
      }
    }
    
    # Power = mean across Monte Carlo replicates
    power_K <- colMeans(detection_matrix, na.rm = TRUE)
    
    results_list[[length(results_list) + 1]] <- tibble::tibble(
      site = site_id,
      sample_size = K,
      species = species_names,
      power = power_K,
      decision_rule = decision_rule
    )
    
    if (K %% 5 == 0 || K == max(n_grid)) {
      message(sprintf("  K=%2d: power range [%.3f, %.3f]", K, min(power_K), max(power_K)))
    }
  }
  
  power_curve <- dplyr::bind_rows(results_list)
  
  # Compute N* (minimum sample size to reach power threshold)
  n_star <- power_curve %>%
    dplyr::group_by(site, species) %>%
    dplyr::filter(power >= power_threshold) %>%
    dplyr::slice_min(sample_size, n = 1, with_ties = FALSE) %>%
    dplyr::select(site, species, n_star = sample_size, power_achieved = power) %>%
    dplyr::mutate(
      power_threshold = power_threshold,
      decision_rule = decision_rule
    ) %>%
    dplyr::ungroup()
  
  # Add species that never reached threshold
  missing_species <- setdiff(species_names, n_star$species)
  if (length(missing_species) > 0) {
    n_star <- dplyr::bind_rows(
      n_star,
      tibble::tibble(
        site = site_id,
        species = missing_species,
        n_star = NA_integer_,
        power_achieved = NA_real_,
        power_threshold = power_threshold,
        decision_rule = decision_rule
      )
    )
  }
  
  # Print N* summary with edge case handling
  ok_n_star <- n_star$n_star[!is.na(n_star$n_star)]
  if (length(ok_n_star) == 0) {
    message(sprintf(
      "[TierP0] N* summary: no species reached %.0f%% power threshold (0/%d species)",
      power_threshold * 100, n_species
    ))
  } else {
    message(sprintf(
      "[TierP0] N* summary: median=%s, range=[%s, %s] (%d/%d species reached %.0f%% power)",
      round(median(ok_n_star), 1),
      min(ok_n_star),
      max(ok_n_star),
      length(ok_n_star),
      n_species,
      power_threshold * 100
    ))
  }
  
  list(
    power_curve = power_curve,
    n_star = n_star,
    meta = list(
      threshold = threshold,
      power_threshold = power_threshold,
      decision_rule = decision_rule,
      posterior_cutoff = if (decision_rule == "binary_detect") posterior_cutoff else NA
    )
  )
}
