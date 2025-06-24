fit_gjam_trend_model <- function(site_data) {
  if (nrow(site_data) == 0) {
    warning("Empty site data passed to fit_gjam_trend_model.")
    return(NULL)
  }
  
  # Prepare data in wide format
  site_data$year <- as.numeric(site_data$year)
  
  abundance_wide <- site_data %>%
    tidyr::pivot_wider(
      id_cols = c(plotID, year, nlcdClass),
      names_from = taxonID,
      values_from = percentCover,
      values_fill = list(percentCover = 0)
    )
  
  # Filter species with non-zero abundance
  species_cols <- setdiff(colnames(abundance_wide), c("plotID", "year", "nlcdClass"))
  species_present <- sapply(abundance_wide[, species_cols], function(x) any(x > 0))
  abundance_wide <- abundance_wide[, c("plotID", "year", "nlcdClass", names(species_present)[species_present])]
  
  if (ncol(abundance_wide) <= 3) {
    warning("No species with non-zero abundance at site.")
    return(NULL)
  }
  
  # Setup GJAM inputs
  y <- abundance_wide[, names(species_present)[species_present]]
  x <- data.frame(year = abundance_wide$year)
  formula <- as.formula(paste("~", paste(colnames(x), collapse = " + ")))
  
  model_list <- list(
    formula = formula,
    ydata = y,
    xdata = x,
    modelList = list(
      typeNames = rep("CA", ncol(y)),
      ng = 1500,
      burnin = 500
    )
  )
  
  suppressMessages({
    fit <- try(gjam::gjam(
      formula = model_list$formula,
      ydata = model_list$ydata,
      xdata = model_list$xdata,
      modelList = model_list$modelList
    ), silent = TRUE)
  })
  
  if (inherits(fit, "try-error")) {
    warning("GJAM trend model failed.")
    return(NULL)
  }
  
  return(fit)
}
