fit_gjam_model <- function(site_data) {
  library(dplyr)
  library(tidyr)
  library(gjam)
  
  site_id <- unique(site_data$siteID)
  
  # 1. Pivot to wide format
  y_wide <- site_data %>%
    tidyr::pivot_wider(
      id_cols = c(siteID, year, plotID, nlcdClass),
      names_from = taxonID,
      values_from = mean_cover,
      values_fill = 0
    )
  
  # 2. Extract predictor matrix
  x_data <- y_wide %>%
    dplyr::select(year, nlcdClass) %>%
    dplyr::mutate(
      year = as.factor(year),
      nlcdClass = as.factor(nlcdClass)
    )
  
  # 3. Extract response matrix
  y_matrix <- y_wide %>%
    dplyr::select(-siteID, -plotID, -year, -nlcdClass)
  
  # 4. Drop zero-sum and zero-variance species
  y_matrix <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]
  zero_var <- apply(y_matrix, 2, function(col) var(col, na.rm = TRUE) == 0)
  y_matrix <- y_matrix[, !zero_var, drop = FALSE]
  
  # 5. Ensure numeric matrix, clean names
  y_matrix <- y_matrix %>%
    dplyr::mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  y_matrix[is.na(y_matrix)] <- 0
  colnames(y_matrix) <- trimws(colnames(y_matrix))
  
  # 6. Confirm row alignment
  if (nrow(x_data) != nrow(y_matrix)) {
    stop("❌ Row mismatch between predictors and responses")
  }
  
  # 7. Fit GJAM model
  message(glue::glue("🔧 Fitting GJAM model for site {site_id}..."))
  formula <- ~ year + nlcdClass
  y_df <- as.data.frame(y_matrix)
  
  model_list <- list(
    typeNames = rep("CA", ncol(y_df)),
    xdata = x_data,
    reductList = list(REDUCT = FALSE),
    ng = 1000,         # increase for production
    burnin = 500
  )
  
  fit <- gjam::gjam(
    formula   = formula,
    xdata     = x_data,
    ydata     = y_df,
    modelList = model_list
  )
  
  # 8. Return clean model object
  return(list(
    fit   = fit,
    site  = site_id,
    xdata = x_data,
    ydata = y_df
  ))
}
