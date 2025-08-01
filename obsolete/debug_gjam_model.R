
# debug_gjam_model.R

# Load libraries
library(dplyr)
library(tidyr)
library(gjam)

# Source your functions
source("R/load_neon_data.R")
source("R/fit_gjam_model.R")

# Define site to debug
site_id <- "JERC"  # Change this to any siteID you want to inspect

# Load NEON data
neon_data <- load_neon_data("data/plant_data.rds")

# Subset data for the site
site_data <- neon_data %>% filter(siteID == site_id)

# Show summary stats before modeling
cat("Summary of raw site data:\n")
print(summary(site_data))
cat("Unique taxa:", length(unique(site_data$taxonID)), "\n")

# Pivot data
y_data <- site_data %>%
  pivot_wider(id_cols = c(siteID, year, plotID, nlcdClass),
              names_from = taxonID,
              values_from = mean_cover,
              values_fill = 0)

# Covariates
x_data <- y_data %>%
  select(year, nlcdClass) %>%
  mutate(year = as.numeric(year),
         nlcdClass = as.factor(nlcdClass))

# Response matrix
y_matrix <- y_data %>%
  select(-siteID, -plotID, -year, -nlcdClass)

# Check response matrix
cat("\nY matrix structure:\n")
print(str(y_matrix))
cat("Any NA in Y matrix? ", anyNA(y_matrix), "\n")
cat("All numeric? ", all(sapply(y_matrix, is.numeric)), "\n")

# Drop zero columns
nonzero_cols <- y_matrix[, colSums(y_matrix, na.rm = TRUE) > 0, drop = FALSE]
cat("Remaining species columns:", ncol(nonzero_cols), "\n")

# Only proceed if valid
if (ncol(nonzero_cols) >= 2) {
  formula <- ~ year + nlcdClass
  model_list <- list(
    formula = formula,
    xdata = x_data,
    ydata = nonzero_cols,
    modelList = list(typeNames = rep("Q", ncol(nonzero_cols)))
  )

  cat("\nFitting GJAM model...\n")
  fit <- tryCatch({
    gjam::gjam(
      formula = model_list$formula,
      xdata = model_list$xdata,
      ydata = model_list$ydata,
      modelList = model_list$modelList
    )
  }, error = function(e) {
    cat("❌ GJAM model error:", e$message, "\n")
    return(NULL)
  })

  if (!is.null(fit)) {
    cat("✅ GJAM model fit successful!\n")
    print(fit$modelSummary)
  }
} else {
  cat("❌ Too few valid species columns. Skipping model.\n")
}
