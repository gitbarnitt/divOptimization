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
  
  # Drop all-zero species
  y_matrix <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]
  
  if (ncol(y_matrix) < 2) {
    message("❌ ", site_id, ": Too few valid species (nonzero cols) — returning NULL.")
    return(NULL)
  }
  
  # Ensure numeric and replace NA with 0
  # Coerce all columns to numeric explicitly before matrix conversion
  y_matrix <- y_matrix %>%
    dplyr::mutate(across(everything(), ~ as.numeric(.))) %>%
    as.matrix()
  
  # Replace NA just in case
  y_matrix[is.na(y_matrix)] <- 0
  
  # Confirm column names are clean
  colnames(y_matrix) <- trimws(colnames(y_matrix))
  
  any(sapply(as.data.frame(y_matrix), is.list))  # Should return FALSE
  
  cat("🧪 Final structure of y_matrix:\n")
  str(y_matrix)
  
  cat("\n🧪 Classes of each column:\n")
  print(sapply(as.data.frame(y_matrix), class))
  
  cat("\n🧪 Unique values in first few columns:\n")
  print(lapply(as.data.frame(y_matrix)[, 1:5], unique))
  
  # Final structure check: confirm all numeric
  if (!all(sapply(as.data.frame(y_matrix), is.numeric))) {
    message("❌ ", site_id, ": Non-numeric columns remain in y_matrix — aborting model.")
    return(NULL)
  }
  
  # Row alignment check
  if (nrow(y_matrix) != nrow(x_data)) {
    message("❌ ", site_id, ": xdata and ydata row mismatch — aborting model.")
    return(NULL)
  }
  
  # Drop columns with zero variance (constant across samples)
  zero_var_cols <- apply(y_matrix, 2, function(col) var(col) == 0)
  if (any(zero_var_cols)) {
    message(paste("🔍", site_id, ": Dropping", sum(zero_var_cols), "zero-variance species"))
    y_matrix <- y_matrix[, !zero_var_cols, drop = FALSE]
  }
  
  #set y to dataframe not matrix
  y_df <- as.data.frame(y_matrix)
  
  #formula <- as.formula("~ factor(year) + nlcdClass")
  formula = ~ year + nlcdClass
  
  #make x_data as.factor since not as such in formula
  x_data <- x_data %>%
    mutate(
      year = as.factor(year), # Ensures alignment with factor(year) in formula
      nlcdClass = as.factor(nlcdClass)
    )
  
  model_list <- list(
    formula = formula,
    xdata = x_data,
    ydata = y_df,
    modelList = list(
      typeNames = rep("CA", ncol(y_df)),
      xdata = x_data, 
      reductList = list(REDUCT = FALSE)  # ✅ <-- Add this line
    )
  ) #Despite what the GJAM documentation says, "Q" (quantitative) type is not accepted unless data are integers or counts. Your data are continuous percent cover values (e.g., 0.375, 2.5), which GJAM expects as "CA" (continuous abundance, i.e. Gaussian).
  
  # Diagnostics
  cat("🧪 y_matrix colnames preview:", paste(head(colnames(y_matrix)), collapse = ", "), "\n")
  cat("🧪 typeNames length:", length(model_list$modelList$typeNames), 
      " | y_matrix cols:", ncol(y_matrix), "\n")
  cat("🧪 Summary of x_data:\n")
  print(summary(x_data))
  
  # Fit the model
  fit <- tryCatch({
    gjam::gjam(
      formula   = model_list$formula,
      xdata     = model_list$xdata,
      ydata     = model_list$ydata,
      modelList = model_list$modelList
    )
  }, error = function(e) {
    message(paste("❌", site_id, ": Error fitting GJAM model:", e$message))
    return(NULL)
  })
  
  # ✅ Assign manual components *after* tryCatch
  if (!is.null(fit)) {
    message("✅ Model fit succeeded. Assigning required components...")
    fit$inputs     <- model_list
    fit$modelList  <- model_list$modelList
    fit$formula    <- model_list$formula
    fit$xdata      <- model_list$xdata
    class(fit)     <- "gjam"
    
    # ✅ Print REDUCT status
    print(fit$modelList$reductList$REDUCT)
  }
  
  # ✅ Return properly
  return(list(
    fit = fit,
    site = site_id,
    xdata = x_data
  ))
}


