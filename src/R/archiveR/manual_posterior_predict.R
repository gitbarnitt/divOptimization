#' Generate Mean-Level Posterior Predictions from GJAM Fit
#'
#' @description
#' Computes latent mean predictions (X*beta) for new data using posterior draws
#' from a fitted GJAM model. This provides Tier 1 (mean-only) predictions without
#' residual noise, useful for understanding expected values and trends.
#'
#' @param fit A fitted GJAM model object containing betaBeta and xdata
#' @param xnew Data frame with predictor columns (year, nlcdClass) to predict
#'
#' @return
#' A numeric array [n_iter, nrow(xnew), n_species] of mean predictions.
#'
#' @details
#' **Formula Construction:**
#'
#' Automatically adapts formula based on nlcdClass variation in training data:
#'   - If >=2 nlcdClass types: ~ year + nlcdClass
#'   - If 1 nlcdClass type: ~ year (nlcdClass excluded)
#'
#' **Factor Level Matching:**
#'
#' Ensures xnew factors match training levels from fit$xdata.
#'
#' **Beta Matrix Format:**
#'
#' Assumes betaBeta is stored as [n_iter, p*S] where:
#'   - p = number of predictors (including intercept)
#'   - S = number of species
#'
#' The function reshapes betaBeta to [p, S] for each posterior draw and computes
#' linear predictors via matrix multiplication: X*beta.
#'
#' @author NEON Optimization Team
#' @date 2025
#'
#' @seealso
#' \code{\link{manual_posterior_predict_obs}} for observation-level predictions
#' with residual noise.
#'
#' @export
manual_posterior_predict <- function(fit, xnew) {
  
  # --------------------------------------------------------------------------
  # 1. Locate Beta Matrix from Fit Object
  # --------------------------------------------------------------------------
  
  betaBeta <- .locate_beta_matrix(fit)  # finds it wherever it lives, logs the path
  
  # --------------------------------------------------------------------------
  # 2. Build Model Matrix with Training Factor Levels
  # --------------------------------------------------------------------------
  
  if (!"xdata" %in% names(fit)) {
    stop("manual_posterior_predict(): 'fit$xdata' not found for factor levels.", call. = FALSE)
  }
  
  # Use stored formula from fit object (ensures consistency with training)
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
      message("[manual_posterior_predict] Using formula without nlcdClass (", n_nlcd_types, " type)")
    }
  }
  
  xnew$year      <- factor(xnew$year,      levels = levels(fit$xdata$year))
  xnew$nlcdClass <- factor(xnew$nlcdClass, levels = levels(fit$xdata$nlcdClass))
  xnew_mm <- model.matrix(model_formula, data = xnew)
  
  p <- ncol(xnew_mm)
  k <- ncol(betaBeta)
  if (k %% p != 0) {
    stop(sprintf("[ERROR] Dimension mismatch: ncol(betaBeta)=%d is not a multiple of ncol(xnew_mm)=%d.", k, p), call. = FALSE)
  }
  n_species <- k %/% p
  
  # --------------------------------------------------------------------------
  # 3. Extract Species Names (or Create Default Names)
  # --------------------------------------------------------------------------
  
  # Try to keep species names if present, otherwise fallback
  species_names <- NULL
  if (!is.null(fit$y) && is.matrix(fit$y)) {
    species_names <- colnames(fit$y)
  } else if (!is.null(fit$ydata)) {
    species_names <- colnames(fit$ydata)
  }
  if (is.null(species_names) || length(species_names) != n_species) {
    species_names <- paste0("sp", seq_len(n_species))
  }
  
  # --------------------------------------------------------------------------
  # 4. Initialize Output Array
  # --------------------------------------------------------------------------
  
  n_iter <- nrow(betaBeta)
  n_new  <- nrow(xnew_mm)
  pred_array <- array(
    NA_real_,
    dim = c(n_iter, n_new, n_species),
    dimnames = list(NULL, rownames(xnew), species_names)
  )
  
  # --------------------------------------------------------------------------
  # 5. Compute Linear Predictors for Each Posterior Draw
  # --------------------------------------------------------------------------
  
  # Compute linear predictor for each draw
  for (i in seq_len(n_iter)) {
    # beta concatenated by species: (p*S) -> p x S
    beta_matrix <- matrix(betaBeta[i, ], nrow = p, ncol = n_species)
    pred_array[i, , ] <- xnew_mm %*% beta_matrix
  }
  
  pred_array  # [iter, new_rows, species]
}
