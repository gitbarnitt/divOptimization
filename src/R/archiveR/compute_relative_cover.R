compute_relative_cover <- function(path) {
  raw <- readRDS(path)
  
  # keep only what we use, early
  filtered <- raw %>%
    dplyr::select(siteID, plotID, year, boutNumber, taxonID, nlcdClass,
                  subplotID, percentCover, samplingImpractical, targetTaxaPresent) %>%
    dplyr::filter(targetTaxaPresent == "Y") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::filter(is.na(samplingImpractical) | samplingImpractical == "OK")
  
  # expected # of subplots per bout (no need to expand rows)
  filtered <- filtered %>%
    dplyr::mutate(n_subplots_expected = dplyr::if_else(year <= 2018L, 8L, 6L),
                  percentCover = tidyr::replace_na(percentCover, 0))
  
  # per-bout totals without expanding to all subplot IDs
  per_bout_summary <- filtered %>%
    dplyr::group_by(siteID, plotID, year, boutNumber, taxonID, nlcdClass, n_subplots_expected) %>%
    dplyr::summarise(
      total_cover = sum(percentCover, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Sum across bouts per plot-year-taxon
  cover_summary <- per_bout_summary %>%
    dplyr::group_by(siteID, plotID, year, taxonID) %>%
    dplyr::summarise(
      total_cover = sum(total_cover, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Plot-year totals
  total_plot_cover <- cover_summary %>%
    dplyr::group_by(siteID, plotID, year) %>%
    dplyr::summarise(
      plot_total_cover = sum(total_cover, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Relative cover with a guard (avoid 0/0)
  relative_cover <- cover_summary %>%
    dplyr::left_join(total_plot_cover, by = c("siteID", "plotID", "year")) %>%
    dplyr::mutate(
      relative_cover = dplyr::if_else(plot_total_cover > 0,
                                      total_cover / plot_total_cover,
                                      NA_real_)
    ) %>%
    dplyr::select(siteID, plotID, year, taxonID, relative_cover)
  
  relative_cover
}
