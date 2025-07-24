fit_gjam_model_test <- function(site_data, n_plots = 10, seed = 123) {
  library(dplyr)
  library(tidyr)
  library(gjam)
  
  set.seed(seed)
  
  site_id <- unique(site_data$siteID)
  
  # ✅ Subset only n plots for faster testing (preserves structure)
  sampled_plots <- unique(site_data$plotID)
  sampled_plots <- sampled_plots[seq_len(min(n_plots, length(sampled_plots)))]
  site_data <- site_data %>% filter(plotID %in% sampled_plots)
  
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
    select(year, nlcdClass) %>%
    mutate(
      year = as.factor(year),
      nlcdClass = as.factor(nlcdClass)
    )
  
  # 3. Extract response matrix
  y_matrix <- y_wide %>%
    select(-siteID, -plotID, -year, -nlcdClass)
  
  # 4. Drop zero-sum and zero-variance species
  y_matrix <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]
  zero_var <- apply(y_matrix, 2, function(col) var(col, na.rm = TRUE) == 0)
  y_matrix <- y_matrix[, !zero_var, drop = FALSE]
  
  # 5. Ensure numeric matrix, clean names
  y_matrix <- y_matrix %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  y_matrix[is.na(y_matrix)] <- 0
  colnames(y_matrix) <- trimws(colnames(y_matrix))
  
  # 6. Confirm row alignment
  if (nrow(x_data) != nrow(y_matrix)) {
    stop("❌ Row mismatch between predictors and responses")
  }
  
  # 7. Fit GJAM model
  message(glue::glue("🔧 Fitting GJAM model (TEST) for site {site_id}..."))
  formula <- ~ year + nlcdClass
  y_df <- as.data.frame(y_matrix)
  
  model_list <- list(
    typeNames = rep("CA", ncol(y_df)),
    xdata = x_data,
    reductList = list(REDUCT = FALSE),
    ng = 1000,
    burnin = 500
  )
  
  fit <- gjam::gjam(
    formula   = formula,
    xdata     = x_data,
    ydata     = y_df,
    modelList = model_list
  )
  
  fit$modelList$reductList$REDUCT <- FALSE
  
  # ✅ Assign important components
  fit$xdata <- x_data
  fit$y <- y_matrix
  fit$typeNames <- model_list$typeNames
  
  # ✅ Add warning if GJAM dropped all species
  if (length(fit$modelList$yNames) == 0) {
    warning("⚠️ GJAM returned no
