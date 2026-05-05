#' Compute MCMC Convergence Diagnostics for GJAM Fit
#'
#' @param fit A fitted GJAM model object with chains
#' @param n_params_check Integer: number of random parameters to check (default: 20)
#'   Set to -1 to check all parameters (slow for large models)
#'
#' @return List with convergence diagnostics:
#'   - ess_beta: Effective sample size for beta parameters (regression coefficients)
#'   - ess_sigma: Effective sample size for sigma parameters (residual covariance)
#'   - n_iter_total: Total MCMC iterations
#'   - n_iter_kept: Iterations kept after burnin
#'   - burnin: Burnin iterations
#'   - ess_summary: Summary statistics of ESS across checked parameters
#'   - warnings: Any convergence warnings
#'
#' @details
#' Uses the coda package to compute effective sample size (ESS) for MCMC chains.
#' ESS measures the number of independent samples from the posterior - values
#' much lower than the total iterations indicate poor mixing/convergence.
#'
#' For large models (many species), checking all parameters is slow. By default,
#' randomly samples 20 beta coefficients and 20 sigma entries for diagnostics.
#'
#' **Interpretation:**
#' - ESS > 200: Generally adequate for inference
#' - ESS > 400: Good convergence
#' - ESS < 100: Concerning - consider increasing iterations or checking for problems
#' - ESS / n_iter_kept < 0.1: Poor mixing - long autocorrelation
#'
#' @author NEON Optimization Team
#' @date 2025
#'
#' @export
compute_convergence_diagnostics <- function(fit, n_params_check = 20) {
  
  if (!requireNamespace("coda", quietly = TRUE)) {
    message("[WARNING] coda package not available - skipping convergence diagnostics")
    return(list(
      warnings = "coda package not installed - cannot compute diagnostics",
      ess_beta = NA,
      ess_sigma = NA,
      n_iter_total = NA,
      n_iter_kept = NA,
      burnin = NA
    ))
  }
  
  diagnostics <- list(
    warnings = character(0),
    ess_beta = numeric(0),
    ess_sigma = numeric(0)
  )
  
  # Extract iteration counts
  n_iter_total <- if (!is.null(fit$modelList$ng)) fit$modelList$ng else NA
  burnin <- if (!is.null(fit$modelList$burnin)) fit$modelList$burnin else NA
  n_iter_kept <- if (!is.na(n_iter_total) && !is.na(burnin)) n_iter_total - burnin else NA
  
  diagnostics$n_iter_total <- n_iter_total
  diagnostics$n_iter_kept <- n_iter_kept
  diagnostics$burnin <- burnin
  
  # --------------------------------------------------------------------------
  # 1. Beta (Regression Coefficient) Diagnostics
  # --------------------------------------------------------------------------
  
  betaBeta <- NULL
  tryCatch({
    betaBeta <- .locate_beta_matrix(fit)
  }, error = function(e) {
    diagnostics$warnings <<- c(diagnostics$warnings, 
                               paste0("betaBeta not found: ", e$message))
  })
  
  if (!is.null(betaBeta) && nrow(betaBeta) > 1) {
    n_beta_params <- ncol(betaBeta)
    
    # Sample parameters to check if many
    if (n_params_check > 0 && n_beta_params > n_params_check) {
      check_indices <- sample(1:n_beta_params, n_params_check)
      beta_subset <- betaBeta[, check_indices, drop = FALSE]
      sampled_note <- sprintf(" (sampled %d of %d)", n_params_check, n_beta_params)
    } else {
      beta_subset <- betaBeta
      check_indices <- 1:n_beta_params
      sampled_note <- sprintf(" (all %d params)", n_beta_params)
    }
    
    # Convert to mcmc object and compute ESS
    tryCatch({
      beta_mcmc <- coda::mcmc(beta_subset)
      ess_vals <- coda::effectiveSize(beta_mcmc)
      
      diagnostics$ess_beta <- ess_vals
      diagnostics$ess_beta_checked_indices <- check_indices
      
      # Summary stats
      diagnostics$ess_beta_summary <- list(
        n_params_checked = length(ess_vals),
        n_params_total = n_beta_params,
        mean = mean(ess_vals, na.rm = TRUE),
        median = median(ess_vals, na.rm = TRUE),
        min = min(ess_vals, na.rm = TRUE),
        max = max(ess_vals, na.rm = TRUE),
        prop_below_100 = mean(ess_vals < 100, na.rm = TRUE),
        prop_below_200 = mean(ess_vals < 200, na.rm = TRUE)
      )
      
      message(sprintf("[Convergence] Beta ESS%s: median=%.0f, range=[%.0f, %.0f]",
                      sampled_note,
                      diagnostics$ess_beta_summary$median,
                      diagnostics$ess_beta_summary$min,
                      diagnostics$ess_beta_summary$max))
      
      # Warnings
      if (diagnostics$ess_beta_summary$median < 200) {
        warning_msg <- sprintf("Low ESS for beta: median=%.0f (recommend ESS>200)", 
                               diagnostics$ess_beta_summary$median)
        diagnostics$warnings <- c(diagnostics$warnings, warning_msg)
        message("[WARNING] ", warning_msg)
      }
      
    }, error = function(e) {
      diagnostics$warnings <<- c(diagnostics$warnings, 
                                 paste0("Failed to compute beta ESS: ", e$message))
    })
  } else {
    diagnostics$warnings <- c(diagnostics$warnings, "betaBeta not available or insufficient iterations")
  }
  
  # --------------------------------------------------------------------------
  # 2. Sigma (Residual Covariance) Diagnostics
  # --------------------------------------------------------------------------
  
  sigmaSave <- NULL
  tryCatch({
    if (!is.null(fit$chains$sigmaSave)) {
      sigmaSave <- fit$chains$sigmaSave
    } else if (!is.null(fit$modelList$sigmaSave)) {
      sigmaSave <- fit$modelList$sigmaSave
    }
  }, error = function(e) {
    diagnostics$warnings <<- c(diagnostics$warnings, 
                               paste0("sigmaSave not found: ", e$message))
  })
  
  if (!is.null(sigmaSave)) {
    # sigmaSave can be [n_iter, S, S] or [n_iter, S*(S+1)/2]
    sigma_dims <- dim(sigmaSave)
    
    if (length(sigma_dims) == 3) {
      # [n_iter, S, S] format
      n_iter_sigma <- sigma_dims[1]
      n_species <- sigma_dims[2]
      
      # Extract diagonal elements (variances) and a few off-diagonals (covariances)
      if (n_iter_sigma > 1) {
        # Sample diagonal elements
        n_check <- min(n_params_check, n_species)
        diag_indices <- sample(1:n_species, n_check)
        
        sigma_subset <- sapply(diag_indices, function(i) sigmaSave[, i, i])
        
        tryCatch({
          sigma_mcmc <- coda::mcmc(sigma_subset)
          ess_vals <- coda::effectiveSize(sigma_mcmc)
          
          diagnostics$ess_sigma <- ess_vals
          diagnostics$ess_sigma_summary <- list(
            n_params_checked = length(ess_vals),
            n_params_total = n_species,
            mean = mean(ess_vals, na.rm = TRUE),
            median = median(ess_vals, na.rm = TRUE),
            min = min(ess_vals, na.rm = TRUE),
            max = max(ess_vals, na.rm = TRUE)
          )
          
          message(sprintf("[Convergence] Sigma ESS (diagonal): median=%.0f, range=[%.0f, %.0f]",
                          diagnostics$ess_sigma_summary$median,
                          diagnostics$ess_sigma_summary$min,
                          diagnostics$ess_sigma_summary$max))
          
        }, error = function(e) {
          diagnostics$warnings <<- c(diagnostics$warnings, 
                                     paste0("Failed to compute sigma ESS: ", e$message))
        })
      }
    } else if (length(sigma_dims) == 2) {
      # [n_iter, S*(S+1)/2] format - lower triangle
      n_iter_sigma <- sigma_dims[1]
      n_sigma_params <- sigma_dims[2]
      
      if (n_iter_sigma > 1) {
        # Sample parameters to check
        n_check <- min(n_params_check, n_sigma_params)
        check_indices <- sample(1:n_sigma_params, n_check)
        sigma_subset <- sigmaSave[, check_indices, drop = FALSE]
        
        tryCatch({
          sigma_mcmc <- coda::mcmc(sigma_subset)
          ess_vals <- coda::effectiveSize(sigma_mcmc)
          
          diagnostics$ess_sigma <- ess_vals
          diagnostics$ess_sigma_summary <- list(
            n_params_checked = length(ess_vals),
            n_params_total = n_sigma_params,
            mean = mean(ess_vals, na.rm = TRUE),
            median = median(ess_vals, na.rm = TRUE),
            min = min(ess_vals, na.rm = TRUE),
            max = max(ess_vals, na.rm = TRUE)
          )
          
          message(sprintf("[Convergence] Sigma ESS: median=%.0f, range=[%.0f, %.0f]",
                          diagnostics$ess_sigma_summary$median,
                          diagnostics$ess_sigma_summary$min,
                          diagnostics$ess_sigma_summary$max))
          
        }, error = function(e) {
          diagnostics$warnings <<- c(diagnostics$warnings, 
                                     paste0("Failed to compute sigma ESS: ", e$message))
        })
      }
    }
  } else {
    diagnostics$warnings <- c(diagnostics$warnings, "sigmaSave not available")
  }
  
  # --------------------------------------------------------------------------
  # 3. Overall Assessment
  # --------------------------------------------------------------------------
  
  if (length(diagnostics$warnings) == 0) {
    message("[Convergence] Diagnostics computed successfully")
  } else {
    message(sprintf("[Convergence] %d warnings generated", length(diagnostics$warnings)))
  }
  
  diagnostics
}
