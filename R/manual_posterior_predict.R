manual_posterior_predict <- function(fit, xnew) {
  betaBeta <- fit$fit$chains$bgibbs
  species <- colnames(fit$ydata)
  n_species <- length(species)
  predictors <- colnames(fit$xdata)
  
  model_formula <- ~ year + nlcdClass
  xnew$year <- factor(xnew$year, levels = levels(fit$xdata$year))
  xnew$nlcdClass <- factor(xnew$nlcdClass, levels = levels(fit$xdata$nlcdClass))
  xnew_mm <- model.matrix(model_formula, data = xnew)
  
  if (ncol(xnew_mm) * n_species != ncol(betaBeta)) {
    stop("❌ Dimension mismatch: xnew_mm × species != betaBeta")
  }
  
  n_iter <- nrow(betaBeta)
  n_new <- nrow(xnew_mm)
  pred_array <- array(NA, dim = c(n_iter, n_new, n_species),
                      dimnames = list(NULL, rownames(xnew), species))
  
  for (i in 1:n_iter) {
    beta_matrix <- matrix(betaBeta[i, ], nrow = ncol(xnew_mm), ncol = n_species)
    pred_array[i,,] <- xnew_mm %*% beta_matrix
  }
  
  return(pred_array)  # [iter, condition, species]
}
