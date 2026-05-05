# R/prune_fit_result.R
prune_fit_result <- function(x, mode = c("conservative", "aggressive")) {
  mode <- match.arg(mode)
  if (!is.list(x) || is.null(x$fit)) return(x)
  
  # Find the beta draws once, via your helpers_chains.R
  beta <- .locate_beta_matrix(x$fit)
  
  if (mode == "aggressive") {
    # Preserve fit$xdata for factor levels used by manual_posterior_predict()
    xdata_fit <- if (!is.null(x$fit$xdata)) x$fit$xdata else NULL
    if (!is.null(xdata_fit)) {
      if ("year" %in% names(xdata_fit))      xdata_fit$year      <- factor(xdata_fit$year)
      if ("nlcdClass" %in% names(xdata_fit)) xdata_fit$nlcdClass <- factor(xdata_fit$nlcdClass)
    }
    
    # Collapse fit to just what we need for downstream prediction
    x$fit <- list(
      betaBeta = beta,
      xdata    = xdata_fit
    )
    
    # Slim *top-level* design data but keep useful IDs for downstream
    if (!is.null(x$xdata)) {
      keep <- intersect(c("siteID", "plotID", "year", "nlcdClass"), names(x$xdata))
      x$xdata <- x$xdata[, keep, drop = FALSE]
      if ("year" %in% names(x$xdata))      x$xdata$year      <- factor(x$xdata$year)
      if ("nlcdClass" %in% names(x$xdata)) x$xdata$nlcdClass <- factor(x$xdata$nlcdClass)
    }
    
    # Drop heavy training responses in aggressive mode only
    x$y     <- NULL
    x$ydata <- NULL
    
  } else {
    # Conservative: keep the existing structure; just attach betaBeta
    x$fit$betaBeta <- beta
    
    # Ensure factor levels exist in fit$xdata (used by manual_posterior_predict)
    if (!is.null(x$fit$xdata)) {
      if ("year" %in% names(x$fit$xdata))      x$fit$xdata$year      <- factor(x$fit$xdata$year)
      if ("nlcdClass" %in% names(x$fit$xdata)) x$fit$xdata$nlcdClass <- factor(x$fit$xdata$nlcdClass)
    }
    # Do not drop y/ydata or other slots in conservative mode.
  }
  
  x
}
