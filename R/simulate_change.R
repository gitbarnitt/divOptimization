simulate_change <- function(
    fit,
    change_year,        # e.g., c("2015", "2016")
    xdata_subset        # subset of fit$xdata with N plots
) {
  # Validate input
  if (!all(change_year %in% levels(fit$xdata$year))) {
    stop("❌ One or more specified years not in model levels.")
  }
  
  # Force factor levels to match model
  year_levels <- levels(fit$xdata$year)
  nlcd_levels <- levels(fit$xdata$nlcdClass)
  
  xdata_subset$year <- factor(xdata_subset$year, levels = year_levels)
  xdata_subset$nlcdClass <- factor(xdata_subset$nlcdClass, levels = nlcd_levels)
  
  # Create covariate matrix for both conditions
  xnew_baseline <- xdata_subset
  xnew_changed  <- xdata_subset
  
  xnew_baseline$year <- factor(change_year[1], levels = year_levels)
  xnew_changed$year  <- factor(change_year[2], levels = year_levels)
  
  # Bind into one xnew
  xnew <- rbind(xnew_baseline, xnew_changed)
  
  # Step 3: Posterior prediction (returns [draws, 2N, species])
  raw_preds <- manual_posterior_predict(fit, xnew)
  
  # Step 4: Average across N plots for each condition
  n_plots <- nrow(xdata_subset)
  preds <- array(NA, dim = c(dim(raw_preds)[1], 2, dim(raw_preds)[3]),
                 dimnames = list(NULL, c("baseline", "changed"), dimnames(raw_preds)[[3]]))
  
  preds[, 1, ] <- apply(raw_preds[, 1:n_plots, ], c(1, 3), mean)
  preds[, 2, ] <- apply(raw_preds[, (n_plots + 1):(2 * n_plots), ], c(1, 3), mean)
  
  return(preds)  # [draws, 2, species]
}
