# R/simulate_yearly_changes.R

simulate_yearly_changes <- function(fit, percent_change = 0.2, nsim = NULL) {
  if (is.null(fit) || is.null(fit$fit)) {
    warning("Empty or failed GJAM fit.")
    return(NULL)
  }
  
  model <- fit$fit
  xdata <- fit$xdata
  ydata <- model$inputs$y
  year_var <- "year"
  
  # Check required variables
  if (!(year_var %in% colnames(xdata))) {
    warning("❌ Missing 'year' column in xdata.")
    return(NULL)
  }
  
  year_levels <- sort(unique(xdata[[year_var]]))
  
  if (length(year_levels) < 2) {
    warning("❌ Not enough years for year-to-year comparisons.")
    return(NULL)
  }
  
  comparisons <- list()
  
  for (i in 1:(length(year_levels) - 1)) {
    year1 <- year_levels[i]
    year2 <- year_levels[i + 1]
    
    x1 <- xdata[xdata[[year_var]] == year1, , drop = FALSE]
    x2 <- xdata[xdata[[year_var]] == year2, , drop = FALSE]
    
    if (nrow(x1) < 1 || nrow(x2) < 1) next
    
    # Align factor levels
    x2$year <- factor(x2$year, levels = levels(model$xdata$year))
    x2$nlcdClass <- factor(x2$nlcdClass, levels = levels(model$xdata$nlcdClass))
    x2 <- x2[, names(model$xdata), drop = FALSE]  # column order match
    
    # Predict posterior draws for year2 observations
    pred_base <- tryCatch({
      posterior_predict_gjam(fit = model, xnew = x2, nsim = nsim)
    }, error = function(e) {
      warning("Prediction failed for year ", year2, ": ", conditionMessage(e))
      return(NULL)
    })
    
    if (!is.null(pred_base)) {
      # Simulate 20% increase in abundance
      pred_sim <- pred_base
      pred_sim$sdList$yMu <- pred_base$sdList$yMu * (1 + percent_change)
      
      comparisons[[paste(year1, year2, sep = "_")]] <- list(
        base_pred = pred_base,
        sim_pred = pred_sim,
        year1 = year1,
        year2 = year2,
        species = colnames(pred_base$sdList$yMu)
      )
    }
  }
  
  return(list(
    site = fit$site,
    comparisons = comparisons
  ))
}
