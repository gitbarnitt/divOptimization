#' Run power analysis across sample sizes
#' 
#' @param sim_data Output from simulate_perturbed_dataset() or equivalent list
#' @param n_grid Integer vector: sample sizes to test
#' @param power_reps Integer: Monte Carlo replicates per sample size
#' @param site_id Character: site identifier
#' @param threshold Numeric: detection threshold as PER-SPECIES RELATIVE PROPORTION
#'                  (default 0.10 = detection if |diff| >= 10% of species' raw cover).
#'                  Computed per-species internally as raw_cover * threshold.
#' @param power_threshold Numeric: threshold for defining N* (default 0.80)
#' @param decision_rule Character: "posterior_prob" or "binary_detect"
#' @param posterior_cutoff Numeric: if decision_rule="binary_detect", threshold for calling detected
#' @param cover_floor Numeric: minimum mean cover (%) for inclusion (default 3.0)
#' @param raw_species_cover Named numeric vector: mean cover per species from raw field data.
#'                  REQUIRED for per-species threshold computation.
#' @param seed Integer: random seed
#' 
#' @return List with power_curve, n_star, community_power, community_nstar, species_cover
run_power_sensitivity <- function(
    sim_data,
    n_grid,
    power_reps,
    site_id,
    threshold = 0.10,
    power_threshold = 0.80,
    decision_rule = c("posterior_prob", "binary_detect"),
    posterior_cutoff = 0.80,
    cover_floor = 3.0,
    raw_species_cover = NULL,
    seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  decision_rule <- match.arg(decision_rule)
  
  y_base <- sim_data$y_base_sim
  y_pert <- sim_data$y_pert_sim
  plot_ids <- sim_data$plot_ids
  n_plots_total <- length(plot_ids)
  n_draws <- dim(y_base)[1]
  n_species <- dim(y_base)[3]
  species_names <- dimnames(y_base)[[3]]
  
  # Cover filter
  if (!is.null(raw_species_cover)) {
    species_mean_cover <- raw_species_cover[species_names]
    species_mean_cover[is.na(species_mean_cover)] <- 0
    message("[TierP0] Cover filter using raw field data cover values")
  } else {
    species_mean_cover <- apply(y_base, 3, mean, na.rm = TRUE)
    message("[TierP0] Cover filter using model-predicted cover (raw not provided)")
  }
  names(species_mean_cover) <- species_names
  
  species_included <- species_mean_cover >= cover_floor
  n_included <- sum(species_included)
  n_excluded <- sum(!species_included)
  
  message(sprintf(
    "[TierP0] Cover filter: %d/%d species above %.1f%% mean cover (%.1f%% excluded)",
    n_included, n_species, cover_floor, n_excluded / n_species * 100
  ))
  
  if (n_included > 0) {
    top_species <- sort(species_mean_cover[species_included], decreasing = TRUE)
    n_show <- min(5, length(top_species))
    message(sprintf("[TierP0] Top %d species by cover: %s", n_show,
      paste(sprintf("%s(%.1f%%)", names(top_species)[1:n_show], top_species[1:n_show]), collapse = ", ")))
  }
  
  if (n_included == 0) {
    warning(sprintf("[TierP0] No species above %.1f%% cover floor at %s.", cover_floor, site_id))
  }
  
  message(sprintf(
    "[TierP0] Power analysis: %d sample sizes x %d reps x %d species (rule=%s, threshold=%.0f%% of raw cover)",
    length(n_grid), power_reps, n_included, decision_rule, threshold * 100
  ))
  
  # Per-species absolute thresholds in percentage points, aligned to species_names
  # e.g. species at 10% cover with threshold=0.10 -> 1 pp absolute threshold
  species_thresholds_all <- species_mean_cover * threshold
  species_thresholds_incl <- species_thresholds_all[species_included]
  
  if (n_included > 0) {
    message(sprintf("[TierP0] Per-species thresholds (pp): median=%.2f, range=[%.2f, %.2f]",
                    median(species_thresholds_incl), min(species_thresholds_incl),
                    max(species_thresholds_incl)))
  }
  
  results_list <- list()
  
  for (K in n_grid) {
    if (K > n_plots_total) {
      warning(sprintf("Skipping K=%d (exceeds available plots=%d)", K, n_plots_total))
      next
    }
    
    if (n_included == 0) {
      results_list[[length(results_list) + 1]] <- tibble::tibble(
        site = site_id, sample_size = K, species = species_names,
        power = NA_real_, mean_cover = species_mean_cover,
        included = species_included, decision_rule = decision_rule
      )
      next
    }
    
    detection_matrix <- matrix(NA_real_, nrow = power_reps, ncol = n_included,
                                dimnames = list(NULL, species_names[species_included]))
    
    for (rep in 1:power_reps) {
      sampled_idx <- sample.int(n_plots_total, size = K, replace = FALSE)
      
      avg_base <- apply(y_base[, sampled_idx, species_included, drop = FALSE], c(1, 3), mean)
      avg_pert <- apply(y_pert[, sampled_idx, species_included, drop = FALSE], c(1, 3), mean)
      abs_diff <- avg_pert - avg_base
      
      # Per-species threshold comparison: abs_diff is [draws x species]; compare
      # each column to its species-specific threshold using sweep
      exceeds <- sweep(abs(abs_diff), 2, species_thresholds_incl, FUN = ">=")
      detect_prob <- colMeans(exceeds, na.rm = TRUE)
      
      if (decision_rule == "posterior_prob") {
        detection_matrix[rep, ] <- detect_prob
      } else {
        detection_matrix[rep, ] <- as.integer(detect_prob >= posterior_cutoff)
      }
    }
    
    power_K_included <- colMeans(detection_matrix, na.rm = TRUE)
    power_K_all <- rep(NA_real_, n_species)
    names(power_K_all) <- species_names
    power_K_all[species_included] <- power_K_included
    
    results_list[[length(results_list) + 1]] <- tibble::tibble(
      site = site_id, sample_size = K, species = species_names,
      power = power_K_all, mean_cover = species_mean_cover,
      included = species_included, decision_rule = decision_rule
    )
    
    if ((K %% 5 == 0 || K == max(n_grid)) && n_included > 0) {
      message(sprintf("  K=%2d: power range [%.3f, %.3f] (%d species)",
                      K, min(power_K_included), max(power_K_included), n_included))
    }
  }
  
  power_curve <- dplyr::bind_rows(results_list)
  
  # Per-species N*
  n_star <- power_curve %>%
    dplyr::filter(included) %>%
    dplyr::group_by(site, species) %>%
    dplyr::filter(power >= power_threshold) %>%
    dplyr::slice_min(sample_size, n = 1, with_ties = FALSE) %>%
    dplyr::select(site, species, n_star = sample_size, power_achieved = power) %>%
    dplyr::mutate(power_threshold = power_threshold, decision_rule = decision_rule) %>%
    dplyr::ungroup()
  
  included_names <- species_names[species_included]
  missing_included <- setdiff(included_names, n_star$species)
  if (length(missing_included) > 0) {
    n_star <- dplyr::bind_rows(n_star, tibble::tibble(
      site = site_id, species = missing_included, n_star = NA_integer_,
      power_achieved = NA_real_, power_threshold = power_threshold, decision_rule = decision_rule
    ))
  }
  
  excluded_names <- species_names[!species_included]
  if (length(excluded_names) > 0) {
    n_star <- dplyr::bind_rows(n_star, tibble::tibble(
      site = site_id, species = excluded_names, n_star = NA_integer_,
      power_achieved = NA_real_, power_threshold = power_threshold,
      decision_rule = paste0(decision_rule, "_below_cover_floor")
    ))
  }
  
  # Cover-weighted community power
  if (n_included > 0) {
    included_covers <- species_mean_cover[species_included]
    cover_weights <- included_covers / sum(included_covers)
    weight_lookup <- setNames(cover_weights, species_names[species_included])
    
    community_power <- power_curve %>%
      dplyr::filter(included) %>%
      dplyr::group_by(site, sample_size) %>%
      dplyr::summarise(
        community_power = sum(power * weight_lookup[species], na.rm = TRUE),
        n_species_evaluated = dplyr::n(),
        n_species_detected = sum(power >= power_threshold, na.rm = TRUE),
        pct_species_detected = mean(power >= power_threshold, na.rm = TRUE) * 100,
        median_power = median(power, na.rm = TRUE),
        .groups = "drop"
      )
    
    community_nstar_val <- community_power %>%
      dplyr::filter(community_power >= power_threshold) %>%
      dplyr::slice_min(sample_size, n = 1, with_ties = FALSE) %>%
      dplyr::pull(sample_size)
    if (length(community_nstar_val) == 0) community_nstar_val <- NA_integer_
  } else {
    community_power <- tibble::tibble()
    community_nstar_val <- NA_integer_
  }
  
  # Summary
  ok_n_star <- n_star %>%
    dplyr::filter(!grepl("below_cover_floor", decision_rule)) %>%
    dplyr::pull(n_star)
  ok_n_star <- ok_n_star[!is.na(ok_n_star)]
  
  if (length(ok_n_star) == 0) {
    message(sprintf("[TierP0] N* summary: no species reached %.0f%% power (0/%d evaluated, %d below cover floor)",
                    power_threshold * 100, n_included, n_excluded))
  } else {
    message(sprintf("[TierP0] N* summary: median=%s, range=[%s, %s] (%d/%d species reached %.0f%% power, %d below cover floor)",
                    round(median(ok_n_star), 1), min(ok_n_star), max(ok_n_star),
                    length(ok_n_star), n_included, power_threshold * 100, n_excluded))
  }
  message(sprintf("[TierP0] Community N* (cover-weighted): %s",
                  if (is.na(community_nstar_val)) "NA (threshold not reached)" else community_nstar_val))
  
  list(
    power_curve = power_curve, n_star = n_star,
    community_power = community_power, community_nstar = community_nstar_val,
    species_cover = tibble::tibble(species = species_names, mean_cover = species_mean_cover,
                                    species_threshold_pp = species_thresholds_all,
                                    included = species_included, cover_floor = cover_floor),
    meta = list(threshold = threshold, threshold_type = "per_species_relative",
                power_threshold = power_threshold, decision_rule = decision_rule,
                posterior_cutoff = if (decision_rule == "binary_detect") posterior_cutoff else NA,
                cover_floor = cover_floor,
                cover_source = if (!is.null(raw_species_cover)) "raw_field_data" else "model_predictions",
                n_species_total = n_species, n_species_included = n_included,
                n_species_excluded = n_excluded)
  )
}
