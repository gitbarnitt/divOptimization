fit_gjam_model <- function(site_data) {
  library(dplyr)
  library(tidyr)
  library(gjam)
  
  site_id <- unique(site_data$siteID)
  
  # Check if data is sufficient
  if (nrow(site_data) < 5 || length(unique(site_data$taxonID)) < 2) {
    message(paste("❌", site_id, ": Insufficient data to fit model. Returning NULL."))
    return(NULL)
  }
  
  # Wide-format matrix of taxon cover
  y_data <- site_data %>%
    tidyr::pivot_wider(
      id_cols = c(siteID, year, plotID, nlcdClass),
      names_from = taxonID,
      values_from = mean_cover,
      values_fill = 0
    )
  
  # Extract covariates
  x_data <- y_data %>%
    dplyr::select(year, nlcdClass) %>%
    mutate(
      year = as.numeric(year),
      nlcdClass = as.factor(nlcdClass)
    )
  
  # Response matrix
  y_matrix <- y_data %>%
    dplyr::select(-siteID, -plotID, -year, -nlcdClass)
  
  if (ncol(y_matrix) < 2) {
    message(paste("❌", site_id, ": Too few species columns to fit GJAM. Returning NULL."))
    return(NULL)
  }
  
  formula <- as.formula(paste("~ year + nlcdClass"))
  
  nonzero_cols <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]
  if (ncol(nonzero_cols) < 2) {
    message("❌ ", site_id, ": Too few valid species (nonzero cols) — returning NULL.")
    return(NULL)
  }
  
  model_list <- list(
    formula = formula,
    xdata = x_data,
    ydata = nonzero_cols,
    modelList = list(typeNames = rep("Q", ncol(nonzero_cols)))
  )
  
  # Just before tryCatch for gjam()
  if (any(is.na(y_matrix))) {
    message("❌ ", site_id, ": y_matrix contains NA values — aborting model.")
    return(NULL)
  }
  
  if (!all(sapply(y_matrix, is.numeric))) {
    message("❌ ", site_id, ": Non-numeric columns in y_matrix — aborting model.")
    return(NULL)
  }
  
  
  # Fit the model
  fit <- tryCatch({
    gjam::gjam(
      formula = model_list$formula,
      xdata = model_list$xdata,
      ydata = model_list$ydata,
      modelList = model_list$modelList
    )
  }, error = function(e) {
    message(paste("❌", site_id, ": Error fitting GJAM model:", e$message))
    return(NULL)
  })
  
  return(fit)
}
