#' Generate Posterior Predictive Draws from GJAM Reduced-Rank Components
#'
#' @description
#' Generates observation-level posterior predictive draws by reconstructing
#' predictions from GJAM's stored reduced-rank components (bgibbs, kgibbs,
#' sgibbs, sigErrGibbs). This approach:
#'
#'   1. Works correctly under dimension reduction
#'   2. Scales to any site size (no giant ygibbs matrix needed)
#'   3. Preserves full inter-species covariance through factor structure
#'   4. Can predict for specific rows (memory efficient)
#'
#' When dimension reduction is NOT active, falls back to extracting from
#' ygibbs if available, or uses bgibbs + sgibbs (full vech) directly.
#'
#' @param fit A fitted GJAM model object
#' @param row_indices Integer vector of observation row indices to predict for.
#'   NULL = all observations (in-sample). Indices refer to rows in fit$inputs$xdata.
#' @param draws Integer vector of posterior draw indices. NULL = all post-burnin draws.
#' @param clamp Logical; clamp predictions to [0, 100]? (default: TRUE)
#' @param seed Optional integer random seed for reproducibility of noise sampling.
#'
#' @return
#' A numeric array [n_draws, length(row_indices), n_species] of posterior
#' predictive draws on the observation scale.
#'
#' @details
#' **Generative model under reduction (Taylor-Rodriguez et al. 2017):**
#'
#'   V_i = B x_i + A w_i + epsilon_i
#'
#' where:
#'   - B (Q x S): regression coefficients from bgibbs
#'   - A = Q(k) Z (S x r): loading matrix, reconstructed from kgibbs and sgibbs
#'   - w_i ~ N(0, I_r): factor scores (r-dimensional, sampled fresh)
#'   - epsilon_i ~ N(0, sigma_eps^2 I_S): isotropic residual from sigErrGibbs
#'
#' This is equivalent to sampling from N(B'x_i, A A' + sigma_eps^2 I) but
#' avoids forming the S x S covariance matrix entirely. For JERC with S=407
#' and r=13, we sample 13-dimensional normals instead of 407-dimensional.
#'
#' **Memory efficiency:**
#'
#' Predictions are generated per-draw in a loop, storing only the requested
#' rows. For tierp0 (30 plots x 2 years = 60 rows) this uses minimal memory.
#' For trendrun (all plot-years), it's still manageable since the bottleneck
#' is n_rows x S per draw, not n_rows x S x n_draws simultaneously.
#'
#' @author NEON Optimization Team
#' @date 2025
#'
#' @export
generate_reduced_predictions <- function(
    fit,
    row_indices = NULL,
    draws       = NULL,
    clamp       = TRUE,
    seed        = NULL
) {

  message("[generate_reduced_predictions v2026-03-26] Loaded")

  if (!is.null(seed)) set.seed(seed)

  # --------------------------------------------------------------------------
  # 1. Determine if Reduction is Active
  # --------------------------------------------------------------------------

  S <- ncol(fit$inputs$y)
  species_names <- colnames(fit$inputs$y)
  full_vech <- S * (S + 1) / 2
  sgibbs_cols <- ncol(fit$chains$sgibbs)
  reduced <- sgibbs_cols < full_vech

  if (!reduced) {
    # No reduction — try ygibbs first, fall back to full Sigma reconstruction
    message("  Full-rank model (no reduction). Checking for ygibbs...")
    if (!is.null(fit$chains$ygibbs)) {
      message("  Using ygibbs directly")
      return(extract_ygibbs_predictions(fit, draws = draws, clamp = clamp))
    } else {
      stop(paste(
        "Full-rank model without ygibbs not yet supported in this function.",
        "Use manual_posterior_predict_obs() or refit with REDUCT=TRUE."
      ))
    }
  }

  # --------------------------------------------------------------------------
  # 2. Extract Dimensions from Reduction
  # --------------------------------------------------------------------------

  # The reduction message tells us N (clusters) and r (factors)
  # sgibbs has N*r columns per draw
  # kgibbs has S columns per draw (cluster assignment per species)
  # sigErrGibbs has 1 value per draw

  kgibbs <- fit$chains$kgibbs
  if (is.null(kgibbs)) stop("kgibbs not found — required for reduced-rank reconstruction")

  sigErrGibbs <- fit$chains$sigErrGibbs
  if (is.null(sigErrGibbs)) stop("sigErrGibbs not found — required for reduced-rank reconstruction")

  bgibbs <- fit$chains$bgibbs
  if (is.null(bgibbs)) stop("bgibbs not found — required for prediction")

  # Determine N and r from sgibbs and reductList
  N_clusters <- 0
  r_factors  <- 0

  if (!is.null(fit$modelList$reductList)) {
    N_clusters <- fit$modelList$reductList$N
    r_factors  <- fit$modelList$reductList$r
  }

  # If reductList is empty, infer from dimensions
  if (N_clusters == 0 || r_factors == 0) {
    # kgibbs values are 1-indexed cluster IDs
    N_clusters <- max(kgibbs, na.rm = TRUE)
    # sgibbs_cols = N * r
    r_factors <- sgibbs_cols / N_clusters
    if (r_factors != round(r_factors)) {
      stop(sprintf(
        "Cannot determine r: sgibbs has %d cols, N=%d clusters gives non-integer r=%.2f",
        sgibbs_cols, N_clusters, r_factors
      ))
    }
    r_factors <- as.integer(r_factors)
  }

  message(sprintf("  Dimension reduction: S=%d, N=%d clusters, r=%d factors", S, N_clusters, r_factors))

  # --------------------------------------------------------------------------
  # 3. Build Design Matrix
  # --------------------------------------------------------------------------

  xStand <- fit$inputs$xStand
  if (is.null(xStand)) stop("fit$inputs$xStand not found")
  Q <- ncol(xStand)  # number of predictors including intercept

  # Validate bgibbs dimensions
  if (ncol(bgibbs) != Q * S) {
    stop(sprintf("bgibbs has %d cols, expected Q*S = %d*%d = %d",
                 ncol(bgibbs), Q, S, Q * S))
  }

  n_obs <- nrow(xStand)

  # --------------------------------------------------------------------------
  # 4. Determine Row and Draw Indices
  # --------------------------------------------------------------------------

  if (is.null(row_indices)) {
    row_indices <- seq_len(n_obs)
  } else {
    if (any(row_indices < 1 | row_indices > n_obs)) {
      stop(sprintf("row_indices out of bounds [1, %d]", n_obs))
    }
  }
  n_rows <- length(row_indices)

  X_sub <- xStand[row_indices, , drop = FALSE]

  n_stored <- nrow(bgibbs)
  if (is.null(draws)) {
    idx <- seq_len(n_stored)
  } else {
    if (any(draws < 1 | draws > n_stored)) {
      stop(sprintf("Draw indices out of bounds [1, %d]", n_stored))
    }
    idx <- draws
  }
  n_draws <- length(idx)

  # --------------------------------------------------------------------------
  # 5. Generate Predictions per Draw
  # --------------------------------------------------------------------------
  #
  # For each draw g:
  #   1. B_g = matrix(bgibbs[g,], Q, S)
  #   2. k_g = kgibbs[g,]  (length-S cluster assignments)
  #   3. Z_g = matrix(sgibbs[g,], N, r)
  #   4. A_g = Q(k_g) %*% Z_g  where Q(k) is the S x N indicator matrix
  #      Equivalently: A_g[s,] = Z_g[k_g[s],]  (just index into Z rows)
  #   5. sigma_eps_g = sigErrGibbs[g]
  #   6. mu_g = X_sub %*% B_g  (n_rows x S)
  #   7. W_new = matrix(rnorm(n_rows * r), n_rows, r)
  #   8. E_new = matrix(rnorm(n_rows * S) * sqrt(sigma_eps_g), n_rows, S)
  #   9. Y_g = mu_g + W_new %*% t(A_g) + E_new
  # --------------------------------------------------------------------------

  pred <- array(NA_real_, dim = c(n_draws, n_rows, S),
                dimnames = list(
                  draw    = NULL,
                  obs     = as.character(row_indices),
                  species = species_names
                ))

  gc_interval <- 100

  for (i in seq_len(n_draws)) {
    g <- idx[i]

    # Extract B for this draw: Q x S
    B_g <- matrix(bgibbs[g, ], nrow = Q, ncol = S)

    # Extract cluster assignments: length S
    k_g <- as.integer(kgibbs[g, ])

    # Extract Z matrix: N x r
    Z_g <- matrix(fit$chains$sgibbs[g, ], nrow = N_clusters, ncol = r_factors)

    # Build A_g: S x r — each species gets the Z row of its cluster
    A_g <- Z_g[k_g, , drop = FALSE]  # S x r

    # Residual variance
    sigma_eps <- sigErrGibbs[g]
    if (!is.finite(sigma_eps) || sigma_eps <= 0) sigma_eps <- 1e-4

    # Mean prediction: n_rows x S
    mu_g <- X_sub %*% B_g

    # Sample factor scores: n_rows x r
    W_new <- matrix(rnorm(n_rows * r_factors), nrow = n_rows, ncol = r_factors)

    # Sample residuals: n_rows x S
    E_new <- matrix(rnorm(n_rows * S) * sqrt(sigma_eps), nrow = n_rows, ncol = S)

    # Combine: Y = mu + W A' + epsilon
    Y_g <- mu_g + W_new %*% t(A_g) + E_new

    # Clamp to observation scale
    if (clamp) {
      Y_g <- pmin(pmax(Y_g, 0), 100)
    }

    pred[i, , ] <- Y_g

    # Periodic GC
    if (i %% gc_interval == 0) {
      rm(B_g, k_g, Z_g, A_g, mu_g, W_new, E_new, Y_g)
      gc(verbose = FALSE)
    }
  }

  message(sprintf("  Returning: %d draws x %d obs x %d species", n_draws, n_rows, S))

  return(pred)
}
