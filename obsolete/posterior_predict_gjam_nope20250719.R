posterior_predict_gjam <- function(fit, xnew, nsim = NULL) {
  if (missing(fit) || !inherits(fit, "gjam")) {
    stop("❌ 'fit' must be a gjam model object")
  }
  if (missing(xnew) || !is.data.frame(xnew)) {
    stop("❌ 'xnew' must be a data.frame of new predictors")
  }
  
  nsim <- nsim %||% fit$modelList$ng
  if (nsim > fit$modelList$ng) {
    warning("Requested 'nsim' exceeds available posterior draws; using all draws.")
    nsim <- fit$modelList$ng
  }
  
  # 🩹 Patch: ensure inputs$u2s is present and safely formed
  if (is.null(fit$inputs$u2s) || !is.logical(attr(fit$inputs$u2s, "valid"))) {
    dummy_u2s <- matrix(0, nrow = 1, ncol = 1)
    attr(dummy_u2s, "valid") <- FALSE
    fit$inputs$u2s <- dummy_u2s
  }
  
  # 🔧 Force REDUCT = FALSE to avoid triggering dimension-reduction logic
  fit$modelList$reductList$REDUCT <- FALSE
  
  pred <- gjam::gjamPredict(
    output = fit,
    newdata = list(xdata = xnew),
    FULL = TRUE
  )
  
  return(pred)
}



#Note: %||% is a safe null-coalescing fallback (you can define it if not already):
#`%||%` <- function(a, b) if (!is.null(a)) a else b

#Output Structure
#test_post_pred$sdList$yMu → Posterior means (rows = new observations, columns = species)

#test_post_pred$sdList$yPost → Posterior draws (array: nsim × obs × species)

#This is exactly what you’ll need to assess whether simulated abundance shifts (from simulate_change()) produce detectable changes in posterior distributions.