test_small_model <- function(site_data) {
  library(dplyr)
  library(tidyr)
  library(gjam)
  
  site_id <- unique(site_data$siteID)
  
  # 1. Convert long to wide (taxa as columns)
  y_data <- site_data %>%
    tidyr::pivot_wider(
      id_cols = c(siteID, year, plotID, nlcdClass),
      names_from = taxonID,
      values_from = mean_cover,
      values_fill = 0
    )
  
  # 2. Extract covariates
  x_data <- y_data %>%
    dplyr::select(year, nlcdClass) %>%
    mutate(
      year = as.factor(year),           # Ensure factor
      nlcdClass = as.factor(nlcdClass)
    )
  
  # 3. Response matrix
  y_matrix <- y_data %>%
    dplyr::select(-siteID, -plotID, -year, -nlcdClass)
  
  # 4. Drop all-zero species columns
  y_matrix <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]
  
  # 5. Drop zero-variance species
  zero_var_cols <- apply(y_matrix, 2, function(col) var(col) == 0)
  if (any(zero_var_cols)) {
    message(paste("🔍 Dropping", sum(zero_var_cols), "zero-variance species"))
    y_matrix <- y_matrix[, !zero_var_cols, drop = FALSE]
  }
  
  # 6. Ensure numeric matrix and clean column names
  y_matrix <- y_matrix %>%
    dplyr::mutate(across(everything(), ~ as.numeric(.))) %>%
    as.matrix()
  
  y_matrix[is.na(y_matrix)] <- 0
  colnames(y_matrix) <- trimws(colnames(y_matrix))
  
  # 7. Row alignment check
  if (nrow(x_data) != nrow(y_matrix)) {
    stop("❌ Row mismatch between x_data and y_matrix")
  }
  
  # 8. Set formula and prepare modelList
  formula <- ~ year + nlcdClass
  y_df <- as.data.frame(y_matrix)
  model_list <- list(
    typeNames = rep("CA", ncol(y_df)),
    xdata = x_data,
    reductList = list(REDUCT = FALSE),
    ng = 100,
    burnin = 10
  )
  
  # 9. Fit GJAM model
  message("🔧 Fitting small test model...")
  fit <- gjam::gjam(
    formula   = formula,
    xdata     = x_data,
    ydata     = y_df,
    modelList = model_list
  )
  
  # 10. Assign manual components for patched prediction compatibility
  fit$inputs     <- list(
    xdata = x_data,
    ydata = y_df,
    formula = formula,
    modelList = model_list
  )
  fit$modelList  <- model_list
  fit$formula    <- formula
  fit$xdata      <- x_data
  fit$xraw       <- x_data
  fit$y          <- y_df
  class(fit)     <- "gjam"
  
  return(fit)
}

###

small_fit <- test_small_model(site_data)

###

xnew <- small_fit$xdata[1:4, , drop = FALSE]
test_pred <- patched_gjamPrediction(
  output = small_fit,
  newdata = list(xdata = xnew),
  y2plot = NULL,
  PLOT = FALSE,
  ylim = NULL,
  FULL = FALSE
)

####

.gjamGetTypes <- function(typeNames) {
  if (is.null(typeNames)) stop('typeNames is NULL')
  
  types <- rep(NA, length(typeNames))
  types[ typeNames == 'CC' ] <- 'continuous'
  types[ typeNames == 'CA' ] <- 'continuous'
  types[ typeNames == 'DA' ] <- 'discrete'
  types[ typeNames == 'PA' ] <- 'binary'
  types[ typeNames == 'OC' ] <- 'ordinal'
  types[ typeNames == 'FC' ] <- 'fractional'
  types[ typeNames == 'CN' ] <- 'constrained'
  types[ typeNames == 'CC' ] <- 'compositional'
  types[ typeNames == 'DI' ] <- 'Dirichlet'
  types[ typeNames == 'RD' ] <- 'rank'
  types[ typeNames == 'TR' ] <- 'trait'
  
  if (any(is.na(types))) {
    stop('Unknown typeName in typeNames: ', paste(typeNames[is.na(types)], collapse = ", "))
  }
  
  return(types)
}

###






xnew <- small_fit$xdata[1:4, , drop = FALSE]

test_pred <- patched_gjamPrediction(
  output = small_fit,
  newdata = list(xdata = xnew),
  y2plot = NULL,
  PLOT = FALSE,
  ylim = NULL,
  FULL = FALSE
)


###
manual_fit <- small_fit
manual_fit$y <- manual_fit$inputs$y
xnew <- manual_fit$xdata[1:4, , drop = FALSE]



# formula <- as.formula("~ year + nlcdClass")
# small_fit$formula <- formula
# small_fit$inputs$formula <- formula
# 
# xnew <- small_fit$xdata[1:4, , drop = FALSE]
# 
# test_pred <- patched_gjamPrediction(
#   output = small_fit,
#   newdata = list(xdata = xnew),
#   y2plot = NULL,
#   PLOT = FALSE,
#   ylim = NULL,
#   FULL = FALSE
# )


