#' Simulate baseline and perturbed datasets using paired posterior predictions
#' 
#' @param fit_result List from fit_gjam_model_test() with $fit, $xdata, $ydata
#' @param year_baseline Character: year for baseline condition (e.g., "2015")
#' @param year_perturbed Character: year for perturbed condition (e.g., "2016")
#' @param effect Numeric: ADDITIVE perturbation in percentage points (e.g., 2.0 = +2pp)
#' @param seed Integer: random seed for reproducibility
#' @param n_draws Integer: how many posterior draws to use (NULL = all)
#' 
#' @details
#' **Perturbation design (critical):**
#' 
#' Both baseline and perturbed predictions are generated for the BASELINE YEAR's
#' plots, using the same MCMC parameter draws (B, A, sigma_eps) but independent
#' observation-level noise (factor scores w and residuals epsilon). This ensures:
#'
#'   1. The ONLY systematic difference is the imposed effect
#'   2. Natural year-to-year variation does not confound the signal
#'   3. Each set has realistic, independent observation noise
#'   4. Parameter uncertainty (B, Sigma) is shared within each paired draw
#'
#' Implementation: `generate_reduced_predictions()` is called TWICE with
#' different random seeds but the same MCMC draw indices. Since the MCMC
#' parameters are read from stored chains (deterministic), both calls get
#' identical B_g, A_g, sigma_g for each draw g. The rnorm() calls for w and
#' epsilon differ because of different seeds, producing independent noise.
#'
#' **Additive perturbation:**
#'
#' The effect is applied as an ADDITIVE shift in percentage points:
#'   Y_pert = Y_base_noise + effect
#' This avoids the denominator problems of multiplicative/relative change.
#'
#' The year_perturbed parameter is used ONLY to determine plot eligibility
#' (plots must be sampled in both years), not for predictions.
#'
#' @return List with:
#'   - y_base_sim: [n_draws, n_plots, n_species] baseline predictions
#'   - y_pert_sim: [n_draws, n_plots, n_species] perturbed predictions
#'   - plot_ids: character vector of plot IDs
#'   - meta: list(effect, seed, years, n_draws, n_plots)
simulate_perturbed_dataset <- function(
    fit_result,
    year_baseline,
    year_perturbed,
    effect = 2.0,
    seed = NULL,
    n_draws = NULL
) {
  if (!is.null(seed)) set.seed(seed)
  
  fit   <- fit_result$fit
  xdata <- fit_result$xdata
  
  # --------------------------------------------------------------------------
  # 1. Validate Years Exist in Training Data
  # --------------------------------------------------------------------------
  
  year_levels <- levels(xdata$year)
  if (!year_baseline %in% year_levels) {
    stop(sprintf("year_baseline '%s' not in model levels: %s", 
                 year_baseline, paste(year_levels, collapse = ", ")))
  }
  if (!year_perturbed %in% year_levels) {
    stop(sprintf("year_perturbed '%s' not in model levels: %s", 
                 year_perturbed, paste(year_levels, collapse = ", ")))
  }
  
  # --------------------------------------------------------------------------
  # 2. Identify Plots Sampled in BOTH Years (Eligible for Comparison)
  # --------------------------------------------------------------------------
  
  plots_baseline <- xdata %>%
    dplyr::filter(year == year_baseline) %>%
    dplyr::distinct(plotID)
  
  plots_perturbed <- xdata %>%
    dplyr::filter(year == year_perturbed) %>%
    dplyr::distinct(plotID)
  
  eligible_plots <- dplyr::inner_join(plots_baseline, plots_perturbed, by = "plotID")
  
  n_base_only <- nrow(plots_baseline) - nrow(eligible_plots)
  n_pert_only <- nrow(plots_perturbed) - nrow(eligible_plots)
  
  if (nrow(eligible_plots) == 0) {
    stop(sprintf(
      "No plots sampled in both %s (n=%d) and %s (n=%d)",
      year_baseline, nrow(plots_baseline),
      year_perturbed, nrow(plots_perturbed)
    ))
  }
  
  if (n_base_only > 0 || n_pert_only > 0) {
    message(sprintf(
      "[TierP0] Restricting to %d plots sampled in both years (excluded: %d baseline-only, %d perturbed-only)",
      nrow(eligible_plots), n_base_only, n_pert_only
    ))
  }
  
  n_plots <- nrow(eligible_plots)
  
  message(sprintf(
    "[TierP0] Simulating %s -> %s with +%.1f pp effect on %d plots (paired counterfactual, additive)",
    year_baseline, year_perturbed, effect, n_plots
  ))
  
  # --------------------------------------------------------------------------
  # 3. Find Row Indices for Baseline Year Plots
  # --------------------------------------------------------------------------
  
  xdata_indexed <- xdata %>%
    dplyr::mutate(.row_idx = dplyr::row_number())
  
  base_rows <- xdata_indexed %>%
    dplyr::semi_join(eligible_plots, by = "plotID") %>%
    dplyr::filter(year == year_baseline) %>%
    dplyr::arrange(plotID)
  
  base_idx <- base_rows$.row_idx
  
  # --------------------------------------------------------------------------
  # 4. Determine Draw Indices
  # --------------------------------------------------------------------------
  
  n_stored <- nrow(fit$chains$bgibbs)
  if (is.null(n_draws) || n_draws > n_stored) {
    draw_idx <- seq_len(n_stored)
  } else {
    draw_idx <- sort(sample.int(n_stored, size = n_draws))
  }
  n_draws_used <- length(draw_idx)
  
  # --------------------------------------------------------------------------
  # 5. Generate Two Independent Prediction Sets
  # --------------------------------------------------------------------------
  
  seed_base <- if (!is.null(seed)) seed * 2 + 1 else sample.int(.Machine$integer.max, 1)
  seed_pert <- if (!is.null(seed)) seed * 2 + 2 else sample.int(.Machine$integer.max, 1)
  
  message(sprintf(
    "[TierP0] Generating paired predictions: %d draws x %d plots (seeds: %d, %d)",
    n_draws_used, n_plots, seed_base, seed_pert
  ))
  
  mu_base <- generate_reduced_predictions(
    fit         = fit,
    row_indices = base_idx,
    draws       = draw_idx,
    clamp       = FALSE,
    seed        = seed_base
  )
  
  mu_pert_raw <- generate_reduced_predictions(
    fit         = fit,
    row_indices = base_idx,
    draws       = draw_idx,
    clamp       = FALSE,
    seed        = seed_pert
  )
  mu_pert <- mu_pert_raw + effect
  rm(mu_pert_raw)
  gc(verbose = FALSE)
  
  # --------------------------------------------------------------------------
  # 6. Return Simulated Datasets
  # --------------------------------------------------------------------------
  
  list(
    y_base_sim = mu_base,
    y_pert_sim = mu_pert,
    plot_ids = as.character(base_rows$plotID),
    meta = list(
      effect          = effect,
      effect_type     = "additive_pp",
      seed            = seed,
      seed_base       = seed_base,
      seed_pert       = seed_pert,
      year_baseline   = year_baseline,
      year_perturbed  = year_perturbed,
      n_draws         = n_draws_used,
      n_plots         = n_plots
    )
  )
}
