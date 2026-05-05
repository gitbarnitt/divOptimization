#' Generate Observation-Level Posterior Predictive Draws with Residual Noise
#'
#' @description
#' Extends manual_posterior_predict() to include residual variation and species
#' covariance from the fitted GJAM model. This provides field-realistic
#' detectability estimates that incorporate both inference uncertainty (beta)
#' and observation-level variability (Sigma).
#'
#' @param fit A fitted GJAM model object containing betaBeta and sigmaSave
#' @param xnew Data frame with predictor columns (year, nlcdClass) as factors
#' @param draws Integer vector of posterior draw indices; NULL = all draws
#' @param eps_sigma Diagonal jitter for PSD safety (default: 1e-4)
#' @param clamp Logical; clamp predictions to [0, 100]? (default: TRUE)
#' @param trace_floor Optional trace coding threshold (e.g., 0.5); NULL = off
#' @param seed Optional random seed for residual sampling reproducibility
#' @param return_mean Logical; also return mean-only (X*beta) predictions?
#'
#' @return
#' A numeric array [n_draws, nrow(xnew), n_species] of predicted observations.
#' If return_mean = TRUE, returns list(mean = ..., obs = ...).
#'
#' @details
#' **Noise Modeling Approach:**
#'
#' This function implements Tier 2 (field-realistic) predictions by adding
#' residual variability using Sigma from sigmaSave. This typically reduces
#' detectability vs Tier 1 (mean-only) and provides more conservative sample
#' size recommendations for operational monitoring.
#'
#' **Covariance Matrix Handling:**
#'
#' The function robustly handles multiple sigmaSave formats from GJAM:
#'   - List of matrices (length n_iter)
#'   - 3D array [n_iter, S, S] or [S, S, n_iter]
#'   - Matrix [n_iter, S*S] or [n_iter, S*(S+1)/2]
#'   - Flat vector (reshapes to 3D)
#'
#' **CA-Specific Censoring:**
#'
#' The CA-specific censoring is approximated via [0,100] clamping. Future
#' refinement may implement GJAM's exact CA likelihood mapping if needed.
#'
#' @author NEON Optimization Team
#' @date 2025
#'
#' @export
manual_posterior_predict_obs <- function(
  fit,
  xnew,
  draws = NULL,
  eps_sigma = 1e-4,
  clamp = TRUE,
  trace_floor = NULL,
  seed = NULL,
  return_mean = FALSE
) {
  
  # Version signature for log verification
  message("[manual_posterior_predict_obs v2026-01-04c] Loaded")
  
  # --------------------------------------------------------------------------
  # 0. Set Random Seed for Reproducibility
  # --------------------------------------------------------------------------
  
  if (!is.null(seed)) set.seed(seed)
  
  # --------------------------------------------------------------------------
  # 1. Construct Model Matrix (X) from New Data
  # --------------------------------------------------------------------------
  
  # Coerce factors to match fit levels
  if (!"year" %in% names(xnew) || !"nlcdClass" %in% names(xnew)) {
    stop("xnew must contain 'year' and 'nlcdClass' columns")
  }
  
  year_levels <- levels(fit$xdata$year)
  nlcd_levels <- levels(fit$xdata$nlcdClass)
  
  xnew$year <- factor(xnew$year, levels = year_levels)
  xnew$nlcdClass <- factor(xnew$nlcdClass, levels = nlcd_levels)
  
  # ---------- Use stored formula from fit object (ensures consistency with training) ----------
  if (!is.null(fit$formula_used)) {
    model_formula <- fit$formula_used
  } else {
    # Fallback: re-derive formula (for backwards compatibility with old fit objects)
    warning("fit$formula_used not found; re-deriving formula from fit$xdata (may be inconsistent)")
    n_nlcd_types <- length(unique(fit$xdata$nlcdClass))
    
    if (n_nlcd_types >= 2) {
      model_formula <- ~ year + nlcdClass
    } else {
      model_formula <- ~ year
      message("[manual_posterior_predict_obs] Using formula without nlcdClass (", n_nlcd_types, " type)")
    }
  }
  
  X <- model.matrix(model_formula, data = xnew)
  p <- ncol(X)
  N <- nrow(X)
  
  # --------------------------------------------------------------------------
  # 2. Extract Posterior Draws of Regression Coefficients (Beta)
  # --------------------------------------------------------------------------
  
  # Step 2: Get posterior draws of β
  betaBeta <- .locate_beta_matrix(fit)
  
  # Determine species info
  species_names <- if (!is.null(colnames(fit$y))) {
    colnames(fit$y)
  } else if (!is.null(colnames(fit$ydata))) {
    colnames(fit$ydata)
  } else {
    stop("Cannot determine species names from fit object")
  }
  
  S <- length(species_names)
  
  # Validate dimensions
  if (ncol(betaBeta) %% p != 0) {
    stop("betaBeta dimensions inconsistent with model matrix")
  }
  implied_S <- ncol(betaBeta) / p
  if (implied_S != S) {
    stop(sprintf("Species count mismatch: betaBeta implies %d, found %d", implied_S, S))
  }
  
  # Select draw indices
  n_iter <- nrow(betaBeta)
  if (is.null(draws)) {
    idx <- seq_len(n_iter)
  } else {
    if (any(draws < 1 | draws > n_iter)) {
      stop(sprintf("Draw indices out of bounds [1, %d]", n_iter))
    }
    idx <- draws
  }
  n_draws_used <- length(idx)
  
  # --------------------------------------------------------------------------
  # 3. Extract and Coerce sigmaSave into [n_iter, S, S] Format
  # --------------------------------------------------------------------------
  
  # ---- Step 3: Get and coerce sigmaSave into [n_iter, S, S] ----
  sigmaSave <- fit$chains$sigmaSave
  if (is.null(sigmaSave)) {
    sigmaSave <- fit$modelList$sigmaSave
  }
  if (is.null(sigmaSave)) {
    stop("sigmaSave not found in fit$chains or fit$modelList - ensure GJAM saved residual covariance")
  }
  
  # Helpful diagnostics
  message(sprintf("[manual_posterior_predict_obs] sigmaSave class: %s", paste(class(sigmaSave), collapse = ",")))
  message(sprintf("[manual_posterior_predict_obs] sigmaSave dim: %s", paste(dim(sigmaSave), collapse = " x ")))
  message(sprintf("[manual_posterior_predict_obs] sigmaSave length: %d", length(sigmaSave)))
  
  # Case 0: Flat numeric vector - try to reshape to [n_iter, S, S]
  if (is.numeric(sigmaSave) && is.null(dim(sigmaSave))) {
    expected_length <- n_iter * S * S
    if (length(sigmaSave) == expected_length) {
      # Reshape assuming column-major order: [S, S, n_iter]
      sigmaSave <- array(sigmaSave, dim = c(S, S, n_iter))
      sigmaSave <- aperm(sigmaSave, c(3, 1, 2))  # Permute to [n_iter, S, S]
      message(sprintf("[manual_posterior_predict_obs] Reshaped flat vector (%d) to [%d, %d, %d]", 
                      expected_length, n_iter, S, S))
      
    } else if (length(sigmaSave) == n_iter) {
      # Interpret as scalar variance per draw -> diagonal covariance per draw
      tmp <- array(0, dim = c(n_iter, S, S))
      for (ii in seq_len(n_iter)) {
        v <- as.numeric(sigmaSave[[ii]])
        if (!is.finite(v) || v <= 0) v <- eps_sigma
        diag(tmp[ii, , ]) <- v
      }
      sigmaSave <- tmp
      message(sprintf("[manual_posterior_predict_obs] Expanded sigmaSave (%d) to diagonal [%d, %d, %d]",
                      n_iter, n_iter, S, S))
      
    } else if (length(sigmaSave) == n_iter * S) {
      # Interpret as per-species diagonal variance per draw
      mat <- matrix(as.numeric(sigmaSave), nrow = n_iter, ncol = S, byrow = TRUE)
      tmp <- array(0, dim = c(n_iter, S, S))
      for (ii in seq_len(n_iter)) {
        v <- mat[ii, ]
        v[!is.finite(v) | v <= 0] <- eps_sigma
        diag(tmp[ii, , ]) <- v
      }
      sigmaSave <- tmp
      message(sprintf("[manual_posterior_predict_obs] Expanded sigmaSave (%d) to per-species diagonal [%d, %d, %d]",
                      n_iter * S, n_iter, S, S))
      
    } else if (length(sigmaSave) == S * S) {
      # Interpret as constant covariance across draws
      M <- matrix(as.numeric(sigmaSave), nrow = S, ncol = S, byrow = TRUE)
      tmp <- array(0, dim = c(n_iter, S, S))
      for (ii in seq_len(n_iter)) tmp[ii, , ] <- M
      sigmaSave <- tmp
      message(sprintf("[manual_posterior_predict_obs] Expanded sigmaSave (%d) to constant [%d, %d, %d]",
                      S * S, n_iter, S, S))
      
    } else {
      stop(sprintf(
        "sigmaSave is a flat vector of length %d, expected %d (n_iter=%d, S=%d) or %d (scalar per draw) or %d (diag per draw) or %d (constant)",
        length(sigmaSave), expected_length, n_iter, S, n_iter, n_iter * S, S * S
      ))
    }
  }
  
  # Case 1: list of matrices (length n_iter)
  if (is.list(sigmaSave)) {
    if (length(sigmaSave) != n_iter) {
      stop(sprintf("sigmaSave list length (%d) != n_iter (%d)", length(sigmaSave), n_iter))
    }
    if (!all(dim(sigmaSave[[1]]) == c(S, S))) {
      stop(sprintf("sigmaSave[[1]] must be %d x %d, got %s",
                   S, S, paste(dim(sigmaSave[[1]]), collapse=" x ")))
    }
    # Convert to array [n_iter, S, S] for consistent downstream indexing
    tmp <- array(NA_real_, dim = c(n_iter, S, S))
    for (ii in seq_len(n_iter)) tmp[ii, , ] <- sigmaSave[[ii]]
    sigmaSave <- tmp
    message("[manual_posterior_predict_obs] Converted list to [n_iter, S, S] array")
  }
  
  # Case 2: 3D array but maybe in [S, S, n_iter]
  if (is.array(sigmaSave) && length(dim(sigmaSave)) == 3) {
    d <- dim(sigmaSave)
    
    # Expected: [n_iter, S, S]
    if (all(d == c(n_iter, S, S))) {
      message("[manual_posterior_predict_obs] sigmaSave already [n_iter, S, S]")
    } else if (all(d == c(S, S, n_iter))) {
      # Common alternative: [S, S, n_iter] -> permute
      sigmaSave <- aperm(sigmaSave, c(3, 1, 2))  # now [n_iter, S, S]
      message("[manual_posterior_predict_obs] Permuted [S, S, n_iter] to [n_iter, S, S]")
    } else {
      stop(sprintf(
        "sigmaSave 3D array has unexpected dim %s; expected [%d,%d,%d] or [%d,%d,%d]",
        paste(d, collapse=" x "), n_iter, S, S, S, S, n_iter
      ))
    }
  }
  
  # Case 3: matrix of draws (n_iter x S*S) or (n_iter x S*(S+1)/2)
  if (is.matrix(sigmaSave) && nrow(sigmaSave) == n_iter) {
    
    # Full matrix stored row-wise: n_iter x (S*S)
    if (ncol(sigmaSave) == S * S) {
      tmp <- array(NA_real_, dim = c(n_iter, S, S))
      for (ii in seq_len(n_iter)) {
        tmp[ii, , ] <- matrix(sigmaSave[ii, ], nrow = S, ncol = S, byrow = TRUE)
      }
      sigmaSave <- tmp
      message("[manual_posterior_predict_obs] Converted matrix [n_iter, S*S] to [n_iter, S, S]")
    }
    
    # Lower triangle (including diag): n_iter x (S*(S+1)/2)
    else if (ncol(sigmaSave) == S * (S + 1) / 2) {
      tmp <- array(NA_real_, dim = c(n_iter, S, S))
      lt <- lower.tri(matrix(0, S, S), diag = TRUE)
      for (ii in seq_len(n_iter)) {
        M <- matrix(0, S, S)
        M[lt] <- sigmaSave[ii, ]
        M <- M + t(M) - diag(diag(M))  # symmetrize
        tmp[ii, , ] <- M
      }
      sigmaSave <- tmp
      message("[manual_posterior_predict_obs] Converted matrix [n_iter, vech] to [n_iter, S, S]")
    }
    
    else {
      stop(sprintf(
        "sigmaSave matrix has %d cols; expected %d (S*S) or %d (S*(S+1)/2)",
        ncol(sigmaSave), S*S, S*(S+1)/2
      ))
    }
  }
  
  # Case 4: Single covariance matrix (NOT ideal)
  if (is.matrix(sigmaSave) && all(dim(sigmaSave) == c(S, S))) {
    stop("sigmaSave is a single SxS matrix (no per-draw covariance). Save Σ draws or switch NOISE_MODE=mu_only.")
  }
  
  # Final validation
  if (!is.array(sigmaSave)) {
    stop(sprintf(
      "sigmaSave must be a list, 3D array, or SxS matrix. Got class=%s, length=%d",
      paste(class(sigmaSave), collapse=","), length(sigmaSave)
    ))
  }
  # ---- End sigmaSave coercion ----
  
  # Initialize output arrays
  pred_obs <- array(NA_real_, dim = c(n_draws_used, N, S),
                    dimnames = list(draw = NULL,
                                    row_id = rownames(xnew) %||% seq_len(N),
                                    species = species_names))
  
  if (return_mean) {
    pred_mean <- array(NA_real_, dim = c(n_draws_used, N, S),
                       dimnames = list(draw = NULL,
                                       row_id = rownames(xnew) %||% seq_len(N),
                                       species = species_names))
  }
  
  # --------------------------------------------------------------------------
  # 5. Generate Predictions for Each Posterior Draw
  # --------------------------------------------------------------------------
  
  # Step 4: Generate predictions for each draw
  # MEMORY FIX: Force GC every 100 draws to prevent accumulation
  gc_interval <- 100
  successful_draws <- 0
  
  for (i in seq_along(idx)) {
    draw_idx <- idx[i]
    
    # Extract β for this draw
    beta_vec <- betaBeta[draw_idx, ]
    beta_matrix <- matrix(beta_vec, nrow = p, ncol = S)
    
    # Compute mean (Xβ)
    mu <- X %*% beta_matrix  # N x S
    
    # Extract Σ for this draw (now always [n_iter, S, S] after coercion)
    Sigma <- sigmaSave[draw_idx, , ]
    
    # Ensure symmetry
    Sigma <- (Sigma + t(Sigma)) / 2
    
    # Add jitter for PSD safety
    Sigma <- Sigma + diag(eps_sigma, S)
    
    # Attempt Cholesky decomposition with retry logic
    max_attempts <- 3
    jitter_multiplier <- 10
    current_jitter <- eps_sigma
    chol_success <- FALSE
    
    for (attempt in seq_len(max_attempts)) {
      R <- tryCatch(
        chol(Sigma),
        error = function(e) NULL
      )
      
      if (!is.null(R)) {
        chol_success <- TRUE
        break
      }
      
      # Increase jitter and retry
      current_jitter <- current_jitter * jitter_multiplier
      Sigma <- (Sigma + t(Sigma)) / 2 + diag(current_jitter, S)
    }
    
    if (!chol_success) {
      # Add diagnostics
      eig <- try(eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values, silent = TRUE)
      eig_info <- if (inherits(eig, "try-error")) "eigenvalue computation failed" else 
                  sprintf("min eigenvalue: %.2e, max eigenvalue: %.2e", min(eig), max(eig))
      
      # Instead of stopping, skip this problematic draw and warn
      warning(sprintf("Skipping draw %d: Cholesky failed after %d attempts (final jitter: %.2e, %s)", 
                      draw_idx, max_attempts, current_jitter, eig_info))
      next  # Skip to next draw
    }
    
    # Generate correlated residuals
    # Z ~ N(0, I), E = Z * R gives E ~ N(0, Σ)
    Z <- matrix(rnorm(N * S), nrow = N, ncol = S)
    E <- Z %*% R
    
    # Add residuals to mean
    Y_lat <- mu + E
    
    # Apply clamping to [0, 100] if requested
    if (clamp) {
      Y_lat <- pmin(pmax(Y_lat, 0), 100)
    }
    
    # Optional trace floor (continuous by default)
    if (!is.null(trace_floor)) {
      # Apply trace threshold: values in (0, trace_floor) → trace_floor
      Y_lat[Y_lat > 0 & Y_lat < trace_floor] <- trace_floor
    }
    
    # Store predictions (increment successful draws counter)
    successful_draws <- successful_draws + 1
    pred_obs[successful_draws, , ] <- Y_lat
    
    # Store mean prediction only for successful draws (keeps mean/obs aligned)
    if (return_mean) {
      pred_mean[successful_draws, , ] <- mu
    }
    
    # Clean up temp objects and force GC periodically
    rm(beta_vec, beta_matrix, mu, Sigma, R, Z, E, Y_lat)
    if (successful_draws %% gc_interval == 0) gc(verbose = FALSE)
  }
  
  # Trim arrays to actual successful draws
  if (successful_draws < n_draws_used) {
    warning(sprintf("Only %d/%d draws succeeded. Trimming output arrays.", successful_draws, n_draws_used))
    pred_obs <- pred_obs[seq_len(successful_draws), , , drop = FALSE]
    if (return_mean) {
      pred_mean <- pred_mean[seq_len(successful_draws), , , drop = FALSE]
    }
  }
  
  if (successful_draws == 0) {
    stop("All MCMC draws failed Cholesky decomposition. Cannot generate predictions.")
  }
  
  # Return
  if (return_mean) {
    return(list(mean = pred_mean, obs = pred_obs))
  } else {
    return(pred_obs)
  }
}

# Helper: null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
