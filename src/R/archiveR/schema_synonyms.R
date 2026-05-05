# R/schema_synonyms.R
SCHEMA_SYNONYMS <- list(
  site        = c("siteID", "site_id"),
  species     = c("taxonID", "taxon_id"),
  mean_detection = c("detect_prob", "detection", "meanDetect", "md"),
  sample_size = c("actual_sample_size", "requested_sample_size", "n_plots"),
  year_pair   = c("yearpair", "pair", "years"),
  year_baseline = c("yearbaseline", "year1", "baseline_year"),
  year_changed = c("yearchanged", "year2", "changed_year"),
  replicate   = c("rep", "replicate_id")
)
MODE_SYNONYMS <- c(
  "variable"           = "sensitivity_variable",
  "sensitivity-var"    = "sensitivity_variable",
  "var"                = "sensitivity_variable",
  "fixed"              = "sensitivity_fixed",
  "base"               = "baseline"
)
