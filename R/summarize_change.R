summarize_change <- function(pred_array, species = NULL) {
  if (is.null(species)) species <- dimnames(pred_array)[[3]][1]
  
  draws <- data.frame(
    baseline = pred_array[, "baseline", species],
    changed  = pred_array[, "changed",  species]
  )
  
  draws$diff <- draws$changed - draws$baseline
  return(draws)
}
