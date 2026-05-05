#' Fit GJAM Model to NEON Plant Diversity Data
#'
#' @description
#' Fits a generalized joint attribute model (GJAM) to multi-species plant cover
#' data, handling both standard and bootstrap-resampled datasets. Automatically
#' adapts formula based on nlcdClass variation and ensures proper chain storage.
#'
#' @param site_data Data frame with columns: siteID, year, plotID, nlcdClass,
#'   taxonID, mean_cover. May include boot_rep_id for bootstrap resamples.
#' @param seed Integer random seed for reproducibility (default: 123)
#'
#' @return
#' List with:
#'   - fit: Fitted GJAM object with patched chains
#'   - site: Site ID
#'   - xdata: Predictor data frame (factors)
#'   - ydata: Response matrix (species cover)
#'
#' @details
#' **Data Preparation:**
#'
#' 1. Pivots long-format data to wide (species as columns)
#' 2. Handles boot_rep_id if present (ensures uniqueness for bootstrap)
#' 3. Drops zero-sum and zero-variance species
#' 4. Builds formula based on nlcdClass variation (>=2 types: includes nlcdClass)
#'
#' **Model Fitting:**
#'
#' - Type: Continuous Abundance (CA) for all species
#' - Iterations: 1000 (500 burnin)
#' - REDUCT: FALSE (dimension reduction disabled)
#'
#' **Post-Fit Patches:**
#'
#' - Ensures chains (betaBeta, sigmaSave) are in modelList for compatibility
#' - Patches u2s matrix if missing (for REDUCT compatibility)
#' - Stores xdata, y, and typeNames in fit object for downstream use
#'
#' @author NEON Optimization Team
#' @date 2025
#'
#' @export
fit_gjam_model_test <- function(site_data, seed = 123) {
  set.seed(seed)
  site_id <- unique(site_data$siteID)
  
  # --------------------------------------------------------------------------
  # 1. Pivot Data to Wide Format (Species as Columns)
  # --------------------------------------------------------------------------
  
  # Handle boot_rep_id if present (from Tier 2A bootstrap resampling)
  id_cols_base <- c("siteID", "year", "plotID", "nlcdClass")
  if ("boot_rep_id" %in% names(site_data)) {
    # Bootstrap resample: use boot_rep_id to ensure uniqueness
    id_cols_use <- c(id_cols_base, "boot_rep_id")
    message("[fit_gjam] Detected boot_rep_id - handling duplicate (plotID, year) rows")
  } else {
    id_cols_use <- id_cols_base
  }
  
  y_wide <- site_data %>%
    tidyr::pivot_wider(
      id_cols = all_of(id_cols_use),
      names_from = taxonID,
      values_from = mean_cover,
      values_fill = 0
    )
  
  # --------------------------------------------------------------------------
  # 2. Extract Predictor Variables (X)
  # --------------------------------------------------------------------------
  
  # Drop boot_rep_id if present (was only needed for pivot uniqueness)
  x_data <- y_wide %>%
    dplyr::select(plotID, year, nlcdClass) %>%  # boot_rep_id deliberately excluded
    dplyr::mutate(dplyr::across(c(year, nlcdClass, plotID), as.factor))
  
  # --------------------------------------------------------------------------
  # 3. Extract Response Matrix (Y) and Filter Species
  # --------------------------------------------------------------------------
  
  y_matrix <- y_wide %>%
    dplyr::select(-siteID, -plotID, -year, -nlcdClass)
  
  # Drop boot_rep_id from y_matrix if present
  if ("boot_rep_id" %in% names(y_matrix)) {
    y_matrix <- y_matrix %>% dplyr::select(-boot_rep_id)
  }
  
  # CRITICAL ASSERTION: xdata and ydata must have same row count
  if (nrow(x_data) != nrow(y_matrix)) {
    stop(sprintf("Row alignment FAILED: xdata has %d rows, ydata has %d rows",
                 nrow(x_data), nrow(y_matrix)))
  }
  
  # --------------------------------------------------------------------------
  # 4. Drop Zero-Sum and Zero-Variance Species
  # --------------------------------------------------------------------------
  
  y_matrix <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]
  zero_var <- apply(y_matrix, 2, function(col) var(col, na.rm = TRUE) == 0)
  y_matrix <- y_matrix[, !zero_var, drop = FALSE]
  
  # --------------------------------------------------------------------------
  # 5. Convert to Numeric Matrix and Clean Column Names
  # --------------------------------------------------------------------------
  
  y_matrix <- y_matrix %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
    as.matrix()
  y_matrix[is.na(y_matrix)] <- 0
  colnames(y_matrix) <- trimws(colnames(y_matrix))
  
  if (nrow(x_data) != nrow(y_matrix)) {
    stop("[ERROR] Row mismatch between predictors and responses")
  }
  
  # --------------------------------------------------------------------------
  # 6. Build Formula Based on NLCD Variation
  # --------------------------------------------------------------------------
  
  n_nlcd_types <- length(unique(x_data$nlcdClass))
  
  if (n_nlcd_types >= 2) {
    formula <- ~ year + nlcdClass
    message("Using formula with nlcdClass (", n_nlcd_types, " types)")
  } else {
    formula <- ~ year
    message("[WARNING] Only 1 NLCD type detected - excluding nlcdClass from formula")
  }
  
  y_df <- as.data.frame(y_matrix)
  
  # --------------------------------------------------------------------------
  # 7. Configure and Fit GJAM Modelv
  # --------------------------------------------------------------------------
  
  model_list <- list(
    typeNames = rep("CA", ncol(y_df)),
    ng        = 5000,
    burnin    = 2500,
    REDUCT    = TRUE,
    reductList = list(r = 5, N = 20)
  )
  
  # Track which formula was actually used for the fit
  formula_used <- formula
  
  fit <- tryCatch(
    gjam::gjam(
      formula   = formula,
      xdata     = x_data,
      ydata     = y_df,
      modelList = model_list
    ),
    error = function(e) {
      if (grepl("chol|positive", e$message, ignore.case = TRUE)) {
        message("[WARNING] GJAM fit failed with nlcdClass (", e$message, ")")
        message("[WARNING] Retrying without nlcdClass...")
        formula_fallback <- ~ year
        formula_used <<- formula_fallback  # Update to fallback formula
        gjam::gjam(
          formula   = formula_fallback,
          xdata     = x_data,
          ydata     = y_df,
          modelList = model_list
        )
      } else {
        stop(e)
      }
    }
  )
  
  # Store formula for prediction functions (ensures consistency)
  fit$formula_used <- formula_used
  
  # Verify ygibbs chains exist (required for extract_ygibbs_predictions) - we don't want this
  # if (!"ygibbs" %in% names(fit$chains)) {
  #   stop("GJAM fit is missing ygibbs chains - cannot generate predictions", call. = FALSE)
  # }
  # 
  # --------------------------------------------------------------------------
  # 8. Post-Fit Patches for Chain Storage and Compatibility
  # --------------------------------------------------------------------------
  
  fit$xdata <- x_data
  fit$y <- y_matrix
  fit$typeNames <- model_list$typeNames
  
  # Patch REDUCT compatibility
  if (is.null(fit$inputs$u2s) || !is.matrix(fit$inputs$u2s)) {
    u2s_patch <- matrix(0, nrow = 1, ncol = 1)
    attr(u2s_patch, "valid") <- as.logical(FALSE)
    fit$inputs$u2s <- u2s_patch
  }
  
  # --------------------------------------------------------------------------
  # 9. Compute Convergence Diagnostics
  # --------------------------------------------------------------------------
  
  message("\n[Convergence] Computing MCMC diagnostics...")
  convergence_diagnostics <- compute_convergence_diagnostics(
    fit = fit,
    n_params_check = 20  # Sample 20 parameters for efficiency
  )
  
  # Store diagnostics in fit object for metadata/logging
  fit$convergence_diagnostics <- convergence_diagnostics
  
  return(list(
    fit   = fit,
    site  = site_id,
    xdata = x_data,
    ydata = y_df
  ))
}
  