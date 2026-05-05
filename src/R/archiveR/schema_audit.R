# R/schema_audit.R
schema_audit <- function(df_norm, name = "species_summary") {
  log <- attr(df_norm, "normalizer_log")
  tibble::tibble(
    table = name,
    canonical = log$canonical,
    mapped_from = log$from,
    status = log$action
  )
}
