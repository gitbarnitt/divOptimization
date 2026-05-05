#' Drop "OTHER" Aggregated Species from Results
#'
#' @description
#' Filters out the "OTHER" synthetic taxon from data frames. The "OTHER" 
#' category aggregates rare species during model fitting to improve numerical
#' stability (reduces S, improves n:S ratio), but should be excluded from
#' species-level results and community-weighted calculations.
#'
#' @param df Data frame or tibble containing species/taxon data
#' @param taxon_col Character. Name of column containing taxon identifiers.
#'   Default is "taxonID". Can also be "species" depending on data structure.
#'
#' @return The input data frame with rows where taxon_col == "OTHER" removed
#'
#' @details
#' This function provides a single point of control for filtering the "OTHER"
#' aggregated taxon. If additional synthetic taxa are added in the future
#' (e.g., "UNKNOWN", "UNIDENTIFIED"), modify this function rather than
#' scattering filter calls throughout the codebase.
#'
#' **Why Filter "OTHER":**
#'
#' - "OTHER" participates in model fitting to reduce dimensionality
#' - "OTHER" should NOT appear in species-level power/trend outputs
#' - "OTHER" should NOT contribute to community-weighted mean (CWM) calculations
#'   because its weight would be the sum of all rare species cover, potentially
#'   very large at high-diversity sites
#'
#' @examples
#' # TierP0 power analysis
#' power_curve_filtered <- drop_other(power_curve_all, "species")
#'
#' # TrendRun species results
#' trend_species_filtered <- drop_other(baseline_species_summary, "species")
#'
#' # Relative cover for CWM
#' cover_filtered <- drop_other(relative_cover_df, "taxonID")
#'
#' @export
drop_other <- function(df, taxon_col = "taxonID") {
  if (!taxon_col %in% names(df)) {
    stop(sprintf(
      "Column '%s' not found in data frame. Available columns: %s",
      taxon_col,
      paste(names(df), collapse = ", ")
    ))
  }
  
  n_before <- nrow(df)
  df_filtered <- dplyr::filter(df, .data[[taxon_col]] != "OTHER")
  n_after <- nrow(df_filtered)
  n_dropped <- n_before - n_after
  
  if (n_dropped > 0) {
    # Include site context in log message if available
    site_context <- ""
    if ("siteID" %in% names(df)) {
      sites <- unique(df$siteID)
      if (length(sites) == 1) {
        site_context <- sprintf(" [site=%s]", sites[1])
      } else if (length(sites) > 1) {
        site_context <- sprintf(" [%d sites]", length(sites))
      }
    } else if ("site" %in% names(df)) {
      sites <- unique(df$site)
      if (length(sites) == 1) {
        site_context <- sprintf(" [site=%s]", sites[1])
      } else if (length(sites) > 1) {
        site_context <- sprintf(" [%d sites]", length(sites))
      }
    }
    
    message(sprintf("  [drop_other]%s Filtered %d rows with %s='OTHER' (%d rows remaining)",
                    site_context, n_dropped, taxon_col, n_after))
  }
  
  df_filtered
}
