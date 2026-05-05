# R/schema_contract.R
CONTRACT_SPECIES_SUMMARY <- c(
  site = "character", species = "character",
  year_baseline = "int", year_changed = "int",
  year_pair = "character",
  sample_size = "int", replicate = "int",
  mean_detection = "numeric"
)

CONTRACT_DRAWS_INDEX <- c(
  site = "character", species = "character",
  year_baseline = "int", year_changed = "int",
  year_pair = "character",
  draw = "int", detected = "logical",
  requested_sample_size = "int", actual_sample_size = "int",
  sample_size = "int", replicate = "int",
  mode = "character"
)
