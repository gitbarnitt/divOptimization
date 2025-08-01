
fit_gjam_model_debug <- function(site_data, inspect = FALSE) {
  library(dplyr)
  library(tidyr)
  library(gjam)

  site_id <- unique(site_data$siteID)
  message("📍 Starting model for site: ", site_id)

  # Check data sufficiency
  if (nrow(site_data) < 5 || length(unique(site_data$taxonID)) < 2) {
    message("❌ ", site_id, ": Insufficient data to fit model. Returning NULL.")
    return(NULL)
  }

  # Wide-format matrix
  y_data <- site_data %>%
    tidyr::pivot_wider(
      id_cols = c(siteID, year, plotID, nlcdClass),
      names_from = taxonID,
      values_from = mean_cover,
      values_fill = 0
    )

  # Covariates
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
    message("❌ ", site_id, ": Too few species columns to fit GJAM. Returning NULL.")
    return(NULL)
  }

  # Drop all-zero species
  y_matrix <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]

  if (ncol(y_matrix) < 2) {
    message("❌ ", site_id, ": Too few valid species (nonzero cols) — returning NULL.")
    return(NULL)
  }

  # Replace NA and enforce numeric mode
  y_matrix[is.na(y_matrix)] <- 0
  y_matrix <- as.matrix(y_matrix)
  mode(y_matrix) <- "numeric"

  # Final structure check
  if (!all(sapply(as.data.frame(y_matrix), is.numeric))) {
    message("❌ ", site_id, ": Non-numeric columns remain in y_matrix — aborting model.")
    return(NULL)
  }

  if (inspect) {
    message("🔍 Inspecting inputs. Opening interactive session...")
    browser()
  }

  formula <- as.formula("~ year + nlcdClass")
  model_list <- list(
    formula = formula,
    xdata = x_data,
    ydata = y_matrix,
    modelList = list(typeNames = rep("Q", ncol(y_matrix)))
  )

  fit <- tryCatch({
    gjam::gjam(
      formula = model_list$formula,
      xdata = model_list$xdata,
      ydata = model_list$ydata,
      modelList = model_list$modelList
    )
  }, error = function(e) {
    message("❌ ", site_id, ": Error fitting GJAM model: ", e$message)
    return(NULL)
  })

  return(fit)
}
