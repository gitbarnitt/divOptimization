library(targets)
library(tarchetypes)

# Source all function scripts in R/
lapply(list.files("R", full.names = TRUE, pattern = "\\.R$"), source)

tar_option_set(
  packages = c("dplyr", "tidyr", "gjam", "purrr", "ggplot2")
)

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
    report,
    render_report(),
    format = "file"
  )
)
