#' Extract Posterior Predictive Draws from a Fitted GJAM Model
#'
#' @description
#' Primary interface for obtaining posterior predictive draws from GJAM.
#' Automatically selects the best available method:
#'
#'   1. If ygibbs exists in chains: reshape and return (fastest, exact)
#'   2. If dimension reduction active: reconstruct from kgibbs, sgibbs,
#'      sigErrGibbs, bgibbs using the Taylor-Rodriguez factor model
#'   3. Otherwise: error (full-rank without ygibbs not supported)
#'
#' Both paths return the same [n_draws, n_obs, n_species] array interface.
#'
#' @param fit A fitted GJAM model object
#' @param row_indices Integer vector of observation row indices to predict for.
#'   NULL = all observations (in-sample). Only used for reduced-rank
#'   reconstruction path; ygibbs path always returns all rows.
#' @param draws Integer vector of posterior draw indices. NULL = all.
#' @param clamp Logical; clamp predictions to [0, 100]? (default: TRUE)
#' @param seed Optional integer random seed (only used for reconstruction path)
#'
#' @return
#' A numeric array [n_draws, n_obs, n_species] of posterior predictive
#' draws on the observation scale.
#'
#' @details
#' At small sites (few observations x species), GJAM stores the full
#' ygibbs matrix during MCMC. At large sites (e.g., JERC with 12K obs x
#' 407 species), ygibbs would exceed available memory and GJAM silently
#' skips storing it. The reconstruction path handles these cases using
#' the generative model:
#'
#'   Y_i = B x_i + A w_i + epsilon_i
#'
#' where A = Q(k)Z is built from stored cluster assignments (kgibbs) and
#' factor loadings (sgibbs), and sigma_eps comes from sigErrGibbs.
#'
#' @seealso \code{\link{generate_reduced_predictions}} for the reconstruction engine
#'
#' @author NEON Optimization Team
#' @date 2025
#'
#' @export
extract_ygibbs_predictions <- function(
    fit,
    row_indices = NULL,
    draws       = NULL,
    clamp       = TRUE,
    seed        = NULL
) {

  message("[extract_ygibbs_predictions v2026-03-26b] Loaded")

  # --------------------------------------------------------------------------
  # 1. Try ygibbs First (small sites where GJAM stored it)
  # --------------------------------------------------------------------------

  ygibbs <- fit$chains$ygibbs

  if (!is.null(ygibbs) && is.matrix(ygibbs) && ncol(ygibbs) > 0) {

    S <- ncol(fit$inputs$y)
    n_obs <- nrow(fit$inputs$xStand)
    expected_cols <- n_obs * S

    if (ncol(ygibbs) == expected_cols) {
      message("  ygibbs found — using stored predictions")
      return(reshape_ygibbs(fit, ygibbs, row_indices, draws, clamp))
    } else {
      message(sprintf("  ygibbs has %d cols (expected %d) — skipping, using reconstruction",
                      ncol(ygibbs), expected_cols))
    }
  } else {
    message("  ygibbs not available — using reduced-rank reconstruction")
  }

  # --------------------------------------------------------------------------
  # 2. Fall Back to Reduced-Rank Reconstruction
  # --------------------------------------------------------------------------

  # Check that reduction components exist
  required <- c("kgibbs", "sgibbs", "sigErrGibbs", "bgibbs")
  missing <- setdiff(required, names(fit$chains))
  if (length(missing) > 0) {
    stop(sprintf(
      "Cannot generate predictions: ygibbs not stored and missing reduction components: %s",
      paste(missing, collapse = ", ")
    ))
  }

  return(generate_reduced_predictions(
    fit         = fit,
    row_indices = row_indices,
    draws       = draws,
    clamp       = clamp,
    seed        = seed
  ))
}


# =============================================================================
# Internal: Reshape ygibbs matrix to [n_draws, n_obs, S] array
# =============================================================================
reshape_ygibbs <- function(fit, ygibbs, row_indices, draws, clamp) {

  S <- ncol(fit$inputs$y)
  species_names <- colnames(fit$inputs$y)
  n_obs <- nrow(fit$inputs$xStand)
  n_stored <- nrow(ygibbs)

  # Select draws
  if (is.null(draws)) {
    idx <- seq_len(n_stored)
  } else {
    if (any(draws < 1 | draws > n_stored)) {
      stop(sprintf("Draw indices out of bounds [1, %d]", n_stored))
    }
    idx <- draws
  }
  n_draws <- length(idx)

  # Select rows
  if (is.null(row_indices)) {
    row_indices <- seq_len(n_obs)
  }
  n_rows <- length(row_indices)

  # Check reduction status (informational)
  sgibbs_cols <- ncol(fit$chains$sgibbs)
  full_vech <- S * (S + 1) / 2
  if (sgibbs_cols < full_vech) {
    message(sprintf("  Dimension reduction active: sgibbs has %d cols (full vech = %d)",
                    sgibbs_cols, full_vech))
    message("  ygibbs predictions incorporate reduced-rank covariance")
  } else {
    message("  Full-rank covariance (no reduction)")
  }

  message(sprintf("  ygibbs: %d draws x %d obs x %d species", n_stored, n_obs, S))

  # Reshape: ygibbs is [n_stored, n_obs * S], column layout is
  # [obs1_sp1, obs1_sp2, ..., obs1_spS, obs2_sp1, ..., obsN_spS]
  pred <- array(NA_real_, dim = c(n_draws, n_rows, S),
                dimnames = list(
                  draw    = NULL,
                  obs     = as.character(row_indices),
                  species = species_names
                ))

  for (i in seq_len(n_draws)) {
    full_mat <- matrix(ygibbs[idx[i], ], nrow = n_obs, ncol = S, byrow = FALSE)
    pred[i, , ] <- full_mat[row_indices, , drop = FALSE]
  }

  if (clamp) {
    pred <- pmin(pmax(pred, 0), 100)
  }

  message(sprintf("  Returning: %d draws x %d obs x %d species", n_draws, n_rows, S))
  return(pred)
}

# Helper: null-coalescing operator
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
