#' Normalize GJAM Chain Names for Consistency
#'
#' @description
#' Standardizes MCMC chain names in GJAM fit objects to ensure consistent
#' access across different GJAM versions. Maps bgibbs -> betaBeta and
#' sigErrGibbs -> sigmaSave if needed.
#'
#' @param fit A fitted GJAM model object with $chains component
#'
#' @return
#' The same fit object with normalized chain names in fit$chains.
#'
#' @details
#' **Chain Name Mapping:**
#'
#' - If betaBeta is missing but bgibbs exists: creates betaBeta = bgibbs
#' - If sigmaSave is missing but sigErrGibbs exists: creates sigmaSave = sigErrGibbs
#'
#' This ensures downstream functions can reliably access:
#'   - fit$chains$betaBeta (regression coefficient posterior draws)
#'   - fit$chains$sigmaSave (residual covariance posterior draws)
#'
#' **Usage:**
#'
#' Call this function immediately after fitting a GJAM model to ensure
#' compatibility with manual_posterior_predict() and manual_posterior_predict_obs().
#'
#' @author NEON Optimization Team
#' @date 2025
#'
#' @export
# R/normalize_gjam_chains.R
normalize_gjam_chains <- function(fit) {
  ch <- fit$chains
  if (!"betaBeta"  %in% names(ch) && "bgibbs"      %in% names(ch)) ch$betaBeta  <- ch$bgibbs
  if (!"sigmaSave" %in% names(ch) && "sigErrGibbs" %in% names(ch)) ch$sigmaSave <- ch$sigErrGibbs
  fit$chains <- ch
  fit
}
