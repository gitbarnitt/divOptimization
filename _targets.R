# _targets.R

library(targets)
library(tarchetypes)

# Source all R scripts in the R/ folder
lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)

# Set global options for the pipeline
tar_option_set(
  packages = c("dplyr", "tidyr", "gjam", "purrr", "ggplot2", "tibble", "readr")
)

# Define the pipeline
list(
  tar_target(
    neon_data,
    load_neon_data("data/plant_data.rds")
  ),
  tar_target(
    site_subsamples,
    split(neon_data, neon_data$siteID)
  ),
  tar_target(
    gjam_fits,
    lapply(site_subsamples, fit_gjam_model)
  ),
  tar_target(
    simulated_fits,
    lapply(gjam_fits, simulate_change)
  ),
  
  tar_target(
    detection_summary,
    estimate_detection_prob(simulated_fits)
  ),
  
  tar_target(
    detection_summary_file,
    {
      dir.create("outputs", showWarnings = FALSE)
      saveRDS(detection_summary, file = "outputs/detection_summary.rds")
      "outputs/detection_summary.rds"
    },
    format = "file"
  ),
  
  tar_target(
    report,
    render_report(),
    format = "file"
  )
)
