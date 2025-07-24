posterior_predict_gjam <- function(fit, xnew, nsim = NULL) {
  # Validate required posterior objects
  betaBeta <- fit$modelList$betaBeta
  sigmaSave <- fit$modelList$sigmaSave
  
  if (is.null(betaBeta) || is.null(sigmaSave)) {
    stop("❌ 'betaBeta' and 'sigmaSave' must be present in fit$modelList.")
  }
  
  # Posterior sample size
  ng <- nrow(betaBeta)
  nsim <- nsim %||% ng
  nsim <- min(nsim, ng)
  
  # Model dimensions
  S <- length(fit$typeNames)
  P <- ncol(betaBeta) / S
  if (P != as.integer(P)) stop("❌ betaBeta shape mismatch: incompatible with y and design matrix")
  P <- as.integer(P)
  expected_sigma_len <- S * S
  if (ncol(sigmaSave) != expected_sigma_len) {
    stop(glue::glue("❌ sigmaSave shape mismatch: expected {expected_sigma_len}, got {ncol(sigmaSave)}"))
  }
  
  # Predictor matrix for new data
  xmat <- model.matrix(fit$modelList$formula, data = xnew)  # [N, P]
  if (ncol(xmat) != P) {
    stop(glue::glue("❌ xnew does not match model predictors (expected {P}, got {ncol(xmat)})"))
  }
  N <- nrow(xmat)
  
  # Allocate prediction array
  y_pred_array <- array(NA_real_, dim = c(N, S, nsim))
  
  # Simulate from posterior
  for (g in 1:nsim) {
    beta_g <- matrix(betaBeta[g, ], nrow = P, ncol = S)
    sigma_g <- matrix(sigmaSave[g, ], nrow = S, ncol = S)
    
    mu <- xmat %*% beta_g  # [N x S]
    for (n in 1:N) {
      y_pred_array[n, , g] <- MASS::mvrnorm(1, mu[n, ], sigma_g)
    }
  }
  
  return(list(
    y_draws = y_pred_array,
    species = colnames(fit$y),
    nsim = nsim,
    site = fit$site
  ))
}



#Note: %||% is a safe null-coalescing fallback (you can define it if not already):
#`%||%` <- function(a, b) if (!is.null(a)) a else b

#Output Structure
#test_post_pred$sdList$yMu → Posterior means (rows = new observations, columns = species)

#test_post_pred$sdList$yPost → Posterior draws (array: nsim × obs × species)

#This is exactly what you’ll need to assess whether simulated abundance shifts (from simulate_change()) produce detectable changes in posterior distributions.