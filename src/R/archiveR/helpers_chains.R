# Robustly find the beta draws matrix in a 'fit' object.
.locate_beta_matrix <- function(fit) {
  # candidate paths relative to 'fit'
  candidates <- list(
    c("betaBeta"),
    c("chains","betaBeta"),
    c("modelList","betaBeta"),
    c("chains","bgibbs"),         # legacy name
    c("fit","chains","bgibbs")    # legacy nested
  )
  
  pluck <- function(x, path) {
    for (nm in path) {
      if (is.null(x) || is.null(x[[nm]])) return(NULL)
      x <- x[[nm]]
    }
    x
  }
  
  for (path in candidates) {
    obj <- pluck(fit, path)
    if (is.matrix(obj) && is.numeric(obj)) {
      used <- paste(c("fit", path), collapse = "$")
      message(sprintf("[manual_posterior_predict] Using beta draws at: %s", used))
      attr(obj, "_used_path") <- used
      return(obj)
    }
  }
  
  stop(
    paste(
      "manual_posterior_predict(): could not locate a numeric matrix of beta draws.",
      "Tried paths:",
      paste(sprintf("fit$%s", vapply(candidates, paste, "", collapse="$")), collapse = ", "),
      sep = "\n  "
    ),
    call. = FALSE
  )
}
