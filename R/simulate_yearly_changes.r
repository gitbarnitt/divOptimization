# R/simulate_yearly_changes.R

simulate_yearly_changes <- function(fit, percent_change = 0.2) {
  if (is.null(fit) || is.null(fit$fit)) {
    warning("Empty or failed GJAM fit.")
    return(NULL)
  }

  model <- fit$fit
  xdata <- model$xdata
  ydata <- model$ydata
  typeNames <- model$typeNames
  year_levels <- sort(unique(xdata$year))

  if (length(year_levels) < 2) {
    warning("Not enough years for year-to-year comparison.")
    return(NULL)
  }

  comparisons <- list()

  for (i in 1:(length(year_levels) - 1)) {
    year1 <- year_levels[i]
    year2 <- year_levels[i + 1]

    x1 <- xdata[xdata$year == year1, , drop = FALSE]
    x2 <- xdata[xdata$year == year2, , drop = FALSE]

    if (nrow(x1) < 1 || nrow(x2) < 1) next

    # Create synthetic newdata with 20% abundance increase in year2
    x2_sim <- x2
    pred_base <- tryCatch({
      gjam::predict.gjam(model, newdata = list(xdata = x2))
    }, error = function(e) {
      warning("Prediction failed for base year: ", conditionMessage(e))
      return(NULL)
    })

    if (is.null(pred_base)) next

    # Simulate 20% increase in expected species abundances
    pred_sim <- pred_base
    pred_sim$sdList$yMu <- pred_base$sdList$yMu * (1 + percent_change)

    # Store results for detection assessment
    comparisons[[paste(year1, year2, sep = "_")]] <- list(
      base_pred = pred_base,
      sim_pred = pred_sim,
      year1 = year1,
      year2 = year2,
      species = colnames(pred_base$sdList$yMu)
    )
  }

  return(list(
    site = fit$site,
    comparisons = comparisons
  ))
}
