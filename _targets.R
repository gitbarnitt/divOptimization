library(targets)
library(tarchetypes)

# Load functions
source("R/load_neon_data.R")
source("R/fit_gjam_model.R")
source("R/simulate_change.R")
source("R/estimate_detection_prob.R")
source("R/plot_detection_prob.R")
source("R/plot_subsampling.R")
source("R/render_report.R")

# Set global packages
tar_option_set(
  packages = c("dplyr", "tidyr", "gjam", "ggplot2", "purrr", "rmarkdown")
)

# Pipeline
list(
  # Load and format NEON data
  tar_target(
    neon_data,
    load_neon_data("data/plant_data.rds")
  ),
  
  # Split by site
  tar_target(
    site_subsamples,
    split(neon_data, neon_data$siteID)
  ),
  
  # Fit GJAM model to each site
  tar_target(
    gjam_fits,
    lapply(site_subsamples, fit_gjam_model)
  ),
  
  # Simulate 20% abundance increase for sensitivity analysis
  tar_target(
    simulated_fits,
    lapply(gjam_fits, simulate_change)
  ),
  
  # Estimate detection probabilities from simulated fits
  tar_target(
    detection_summary,
    estimate_detection_prob(simulated_fits)
  ),
  
  # Render report using detection summary
  tar_target(
    report,
    render_report(),
    format = "file"
  )
)
