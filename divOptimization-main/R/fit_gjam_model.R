fit_gjam_model <- function(site_data) {
  library(dplyr)
  library(tidyr)
  library(gjam)
  
  # Check if data is sufficient
  if (nrow(site_data) < 5 || length(unique(site_data$taxonID)) < 2) {
    message("Insufficient data to fit model. Returning NULL.")
    return(NULL)
  }
  
  # Wide-format matrix of taxon cover: one row per plotID-year-nlcdClass
  y_data <- site_data %>%
    tidyr::pivot_wider(
      id_cols = c(siteID, year, plotID, nlcdClass),
      names_from = taxonID,
      values_from = mean_cover,
      values_fill = 0
    )
  
  # Extract covariates and response matrix
  x_data <- y_data %>%
    dplyr::select(year, nlcdClass) %>%
    mutate(
      year = as.numeric(year),
      nlcdClass = as.factor(nlcdClass)
    )
  
  y_matrix <- y_data %>%
    dplyr::select(-siteID, -plotID, -year, -nlcdClass)
  
  if (ncol(y_matrix) < 2) {
    message("Too few species columns to fit GJAM. Returning NULL.")
    return(NULL)
  }
  
  # Model formula
  formula <- as.formula(paste("~ year + nlcdClass"))
  
  # Model list for GJAM
  model_list <- list(
    formula = formula,
    xdata = x_data,
    ydata = y_matrix,
    modelList = list(typeNames = rep("Q", ncol(y_matrix)))
  )
  
  # Fit the model
  fit <- tryCatch({
    gjam::gjam(
      formula = model_list$formula,
      xdata = model_list$xdata,
      ydata = model_list$ydata,
      modelList = model_list$modelList
    )
  }, error = function(e) {
    message("Error fitting GJAM model: ", e$message)
    return(NULL)
  })
  
  return(fit)
}
