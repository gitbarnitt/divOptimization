# R/simulate_change.R

simulate_change <- function(fit) {
  if (is.null(fit) || is.null(fit$fit)) {
    warning("Empty or failed GJAM fit.")
    return(NULL)
  }
  
  model <- fit$fit
  xdata <- model$xdata
  if (!("year" %in% names(xdata))) {
    warning("No 'year' variable found in model covariates.")
    return(NULL)
  }
  
  # Identify unique years and simulate a +20% change in the latest year
  year_range <- sort(unique(xdata$year))
  if (length(year_range) < 2) {
    warning("Only one year of data present; skipping simulation.")
    return(NULL)
  }
  
  year_new <- max(year_range)
  year_old <- min(year_range)
  
  # Create a new prediction dataset simulating a 20% increase in the latest year
  x_pred <- xdata
  x_pred$year <- year_new
  
  pred <- tryCatch({
    gjam::predict.gjam(model, newdata = list(xdata = x_pred))
  }, error = function(e) {
    warning("Prediction failed: ", conditionMessage(e))
    return(NULL)
  })
  
  list(
    site = fit$site,
    old_year = year_old,
    new_year = year_new,
    pred = pred
  )
}
