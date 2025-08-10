# R/normalize_gjam_chains.R
normalize_gjam_chains <- function(fit) {
  ch <- fit$chains
  if (!"betaBeta"  %in% names(ch) && "bgibbs"      %in% names(ch)) ch$betaBeta  <- ch$bgibbs
  if (!"sigmaSave" %in% names(ch) && "sigErrGibbs" %in% names(ch)) ch$sigmaSave <- ch$sigErrGibbs
  fit$chains <- ch
  fit
}
