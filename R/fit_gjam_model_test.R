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
  
  # ---------- Pivot to wide ----------
  y_wide <- site_data %>%
    tidyr::pivot_wider(
      id_cols = c(siteID, year, plotID, nlcdClass),
      names_from = taxonID,
      values_from = mean_cover,
      values_fill = 0
    )
  
  # ---------- Extract predictors ----------
  x_data <- y_wide %>%
    select(year, nlcdClass) %>%
    mutate(year = as.factor(year),
           nlcdClass = as.factor(nlcdClass))
  
  # ---------- Extract response matrix ----------
  y_matrix <- y_wide %>%
    select(-siteID, -plotID, -year, -nlcdClass)
  
  # ---------- Drop zero-sum and zero-variance species ----------
  y_matrix <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]
  zero_var <- apply(y_matrix, 2, function(col) var(col, na.rm = TRUE) == 0)
  y_matrix <- y_matrix[, !zero_var, drop = FALSE]
  
  # ---------- Convert to numeric matrix ----------
  y_matrix <- y_matrix %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  y_matrix[is.na(y_matrix)] <- 0
  colnames(y_matrix) <- trimws(colnames(y_matrix))
  
  if (nrow(x_data) != nrow(y_matrix)) {
    stop("❌ Row mismatch between predictors and responses")
  }
  
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
  
  # 🔥 Manually patch missing chains into modelList
  fit$modelList$betaBeta <- fit$chains$bgibbs
  fit$modelList$sigmaSave <- fit$chains$sgibbs
  
  fit$xdata <- x_data
  fit$y <- y_matrix
  fit$typeNames <- model_list$typeNames
  
  # 🔧 Patch REDUCT compatibility
  if (is.null(fit$inputs$u2s) || !is.matrix(fit$inputs$u2s)) {
    u2s_patch <- matrix(0, nrow = 1, ncol = 1)
    attr(u2s_patch, "valid") <- as.logical(FALSE)
    fit$inputs$u2s <- u2s_patch
  }
  
  return(list(
    fit   = fit,
    site  = site_id,
    xdata = x_data,
    ydata = y_df
  ))
}
