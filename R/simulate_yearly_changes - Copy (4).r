# R/simulate_yearly_changes.R

simulate_yearly_changes <- function(fit, percent_change = 0.2) {
  if (is.null(fit) || is.null(fit$fit)) {
    warning("Empty or failed GJAM fit.")
    return(NULL)
  }
  
  model <- fit$fit
  xdata <- fit$xdata  # use xdata from model fit to ensure alignment
  ydata <- model$inputs$y  # original response data
  typeNames <- model$typeNames
  
  cat("✅ simulate_yearly_changes: class(xdata): ", class(xdata), "\n")
  cat("✅ simulate_yearly_changes: names(xdata): ", paste(names(xdata), collapse = ", "), "\n")
  cat("✅ simulate_yearly_changes: str(xdata$year):\n")
  str(xdata$year)
  
  if (!("year" %in% colnames(xdata))) {
    warning("❌ Year column missing or xdata malformed. xdata class: ", class(xdata), ", names: ", paste(names(xdata), collapse = ", "))
    return(NULL)
  }
  
  year_levels <- sort(unique(xdata$year))
  
  if (length(year_levels) < 2) {
    warning("Not enough years for year-to-year comparison.")
    return(NULL)
  }
  
  comparisons <- list()
  
  for (i in 1:(length(year_levels) - 1)) {
    year1 <- year_levels[i]
    year2 <- year_levels[i + 1]
    
    x1 <- xdata[xdata$year == year1, , drop = FALSE]
    x2 <- xdata[xdata$year == year2, , drop = FALSE]
    
    if (nrow(x1) < 1 || nrow(x2) < 1) next
    
    ## ---------- STEP 1: DIAGNOSTICS ----------
    cat("\n🔍 Structure of x2:\n")
    str(x2)
    
    cat("\n🔍 Unique values of x2$year:\n")
    print(unique(x2$year))
    
    cat("\n🔍 Class of x2$year:\n")
    print(class(x2$year))
    
    cat("\n🔍 Summary of all x2 columns:\n")
    print(summary(x2))
    
    cat("\n🔍 Structure of model$xdata:\n")
    str(model$xdata)
    ## -----------------------------------------
    
    ## ---------- STEP 2: ALIGN FACTOR LEVELS ----------
    x2$year <- factor(x2$year, levels = levels(model$xdata$year))
    x2$nlcdClass <- factor(x2$nlcdClass, levels = levels(model$xdata$nlcdClass))
    ## -------------------------------------------------
    
    ## ---------- STEP 3: OPTIONAL - ALIGN COLUMN ORDER ----------
    if (!identical(names(x2), names(model$xdata))) {
      cat("🛠 Reordering columns in x2 to match model$xdata\n")
      x2 <- x2[, names(model$xdata), drop = FALSE]
    }
    ## ------------------------------------------------------------
    
    if (is.null(model$inputs)) {
      warning("model$inputs is NULL. gjamPredict will likely fail.")
    }
    
    # 🔍 Debugging: inspect the model before prediction
    cat("🔍 Checking model structure before prediction:\n")
    str(model)
    cat("🔍 model$inputs present:", !is.null(model$inputs), "\n")
    cat("🔍 model$modelList present:", !is.null(model$modelList), "\n")
    cat("🔍 model$parameters present:", !is.null(model$parameters), "\n")
    cat("🔍 model$formula present:", !is.null(model$formula), "\n")
    
    cat("🔬 Verifying x2 just before prediction:\n")
    str(x2)
    cat("🔬 Does x2 match model$xdata names?\n")
    print(setequal(names(x2), names(model$xdata)))
    cat("🔬 Checking for NAs in x2:\n")
    print(colSums(is.na(x2)))
    
    cat("🔬 Inspecting model$modelList:\n")
    str(model$modelList)
    
    # Predict base (original) species abundances in year2
    pred_base <- tryCatch({
      gjam::gjamPredict(output = model, newdata = list(xdata = x2))
    }, error = function(e) {
      warning("Prediction failed for base year: ", conditionMessage(e))
      return(NULL)
    })
    
    if (!is.null(pred_base)) {
      # Simulate 20% increase in predicted abundances
      pred_sim <- pred_base
      pred_sim$sdList$yMu <- pred_base$sdList$yMu * (1 + percent_change)
      
      # Store the result
      message("✅ Storing comparison for: ", year1, " → ", year2)
      comparisons[[paste(year1, year2, sep = "_")]] <- list(
        base_pred = pred_base,
        sim_pred = pred_sim,
        year1 = year1,
        year2 = year2,
        species = colnames(pred_base$sdList$yMu)
      )
    } else {
      message("⚠️  pred_base is NULL for ", year1, " → ", year2)
    }
  }
  
  return(list(
    site = fit$site,
    comparisons = comparisons
  ))
}
