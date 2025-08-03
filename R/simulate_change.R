simulate_change <- function(
    fit,
    change_year,       # <-- must now be provided explicitly
    plot_index = 1
) {
  # Validate input
  if (!all(change_year %in% levels(fit$xdata$year))) {
    stop("❌ One or more specified years not in model levels.")
  }
  
  # Step 1: Use a reference plot from xdata
  ref_row <- fit$xdata[plot_index, ]
  
  # Step 2: Build new covariate matrix for two conditions
  xnew <- as.data.frame(ref_row[rep(1, 2), ])
  xnew$year <- factor(change_year, levels = levels(fit$xdata$year))
  xnew$nlcdClass <- factor(xnew$nlcdClass, levels = levels(fit$xdata$nlcdClass))
  rownames(xnew) <- c("baseline", "changed")
  
  # Step 3: Predict
  preds <- manual_posterior_predict(fit, xnew)
  return(preds)  # dims: [draws, condition (2), species]
}
