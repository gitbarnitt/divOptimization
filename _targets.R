# _targets.R

library(targets)
library(tarchetypes)

# Load custom functions
source("R/load_neon_data.R")
source("R/fit_gjam_model.R")
source("R/fit_gjam_trend_model.R")
source("R/simulate_change.R")
source("R/estimate_detection_prob.R")
source("R/plot_detection_prob.R")
source("R/plot_subsampling.R")
source("R/render_report.R")

# Set package dependencies
tar_option_set(packages = c("dplyr", "tidyr", "gjam", "purrr", "ggplot2", "rmarkdown"))

# Define pipeline
list(
  # Load and split data
  tar_target(neon_data, load_neon_data("data/plant_data.rds")),
  tar_target(site_subsamples, split(neon_data, neon_data$siteID)),
  
  # Fit year-to-year GJAM models
  tar_target(gjam_fits, lapply(site_subsamples, fit_gjam_model)),
  
  # Fit trend models
  tar_target(gjam_trend_fits, lapply(site_subsamples, fit_gjam_trend_model)),
  
  # Simulate detection
  tar_target(simulated_fits, lapply(gjam_fits, simulate_change)),
  
  # Estimate detection probability
  tar_target(detection_summary, estimate_detection_prob(simulated_fits)),
  
  # Generate report
  tar_target(
    report,
    render_report(),
    format = "file"
  )
)
