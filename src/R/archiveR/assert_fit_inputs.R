#' Validation helpers for Tier 2 (strict shape/alignment checks) this
#'
#' @name tier2_validation
#' @details
#' These functions enforce strict contracts to prevent silent drift in Tier 2.
#' All fail early with informative errors.

#' Assert xdata and ydata are properly aligned
#'
#' @param xdata data.frame with plotID, year, covariates
#' @param ydata data.frame with plotID, year, species columns
#' @param require_cols optional character vector of required column names
assert_fit_inputs <- function(xdata, ydata, require_cols = NULL) {
  
  # Basic structure
  if (!is.data.frame(xdata)) {
    stop("assert_fit_inputs(): xdata must be a data.frame")
  }
  if (!is.data.frame(ydata)) {
    stop("assert_fit_inputs(): ydata must be a data.frame")
  }
  
  # Row count
  if (nrow(xdata) != nrow(ydata)) {
    stop(sprintf("assert_fit_inputs(): xdata has %d rows, ydata has %d rows (must match)",
                 nrow(xdata), nrow(ydata)))
  }
  
  if (nrow(xdata) == 0) {
    stop("assert_fit_inputs(): xdata/ydata are empty")
  }
  
  # Required keys
  if (!"plotID" %in% names(xdata)) {
    stop("assert_fit_inputs(): xdata missing 'plotID' column")
  }
  if (!"plotID" %in% names(ydata)) {
    stop("assert_fit_inputs(): ydata missing 'plotID' column")
  }
  if (!"year" %in% names(xdata)) {
    stop("assert_fit_inputs(): xdata missing 'year' column")
  }
  if (!"year" %in% names(ydata)) {
    stop("assert_fit_inputs(): ydata missing 'year' column")
  }
  
  # Key alignment
  if (!identical(xdata$plotID, ydata$plotID)) {
    stop("assert_fit_inputs(): xdata$plotID and ydata$plotID must be identical (same order)")
  }
  if (!identical(xdata$year, ydata$year)) {
    stop("assert_fit_inputs(): xdata$year and ydata$year must be identical (same order)")
  }
  
  # Optional additional columns
  if (!is.null(require_cols)) {
    missing_x <- setdiff(require_cols, names(xdata))
    if (length(missing_x) > 0) {
      stop(sprintf("assert_fit_inputs(): xdata missing required columns: %s",
                   paste(missing_x, collapse = ", ")))
    }
  }
  
  invisible(TRUE)
}


#' Assert species columns match between ydata and fit
#'
#' @param ydata data.frame with species columns
#' @param fit gjam fit object
#' @param species_cols character vector of expected species names (optional)
assert_species_order <- function(ydata, fit, species_cols = NULL) {
  
  # Get species from fit
  if (is.null(fit$modelList$S)) {
    stop("assert_species_order(): fit$modelList$S is NULL (invalid fit object)")
  }
  
  fit_species <- colnames(fit$modelList$y)
  if (is.null(fit_species)) {
    stop("assert_species_order(): fit$modelList$y has no column names")
  }
  
  # Get species from ydata (exclude plotID, year)
  ydata_species <- setdiff(names(ydata), c("plotID", "year"))
  
  # Check match
  if (!identical(ydata_species, fit_species)) {
    missing_in_fit <- setdiff(ydata_species, fit_species)
    missing_in_ydata <- setdiff(fit_species, ydata_species)
    
    msg <- "assert_species_order(): species mismatch between ydata and fit"
    if (length(missing_in_fit) > 0) {
      msg <- paste0(msg, sprintf("\n  In ydata but not fit: %s", 
                                  paste(head(missing_in_fit, 5), collapse = ", ")))
    }
    if (length(missing_in_ydata) > 0) {
      msg <- paste0(msg, sprintf("\n  In fit but not ydata: %s",
                                  paste(head(missing_in_ydata, 5), collapse = ", ")))
    }
    stop(msg)
  }
  
  # Optional: check against expected species list
  if (!is.null(species_cols)) {
    if (!identical(ydata_species, species_cols)) {
      stop(sprintf("assert_species_order(): ydata species do not match expected: expected %d, got %d",
                   length(species_cols), length(ydata_species)))
    }
  }
  
  invisible(TRUE)
}


#' Assert no silent drops in dimensions
#'
#' @param expected_n expected number of rows
#' @param actual_n actual number of rows
#' @param context character description of what's being checked
assert_no_silent_drop <- function(expected_n, actual_n, context = "data") {
  if (actual_n < expected_n) {
    stop(sprintf("assert_no_silent_drop(): %s lost rows: expected %d, got %d",
                 context, expected_n, actual_n))
  }
  invisible(TRUE)
}
