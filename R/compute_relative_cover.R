compute_relative_cover <- function(path) {
  raw <- readRDS(path)
  
  subplot_lists <- list(
    before_2019 = c("31_1_1", "32_1_4", "32_1_2", "40_1_3", "40_1_1", "41_1_1", "41_1_4", "31_1_4"),
    after_2018  = c("31_1_1", "32_1_4", "32_1_2", "40_1_3", "40_1_1", "41_1_4")
  )
  
  filtered <- raw %>%
    dplyr::filter(targetTaxaPresent == "Y") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::filter(is.na(samplingImpractical) | samplingImpractical == "OK")
  
  complete_grid <- filtered %>%
    dplyr::distinct(siteID, plotID, year, boutNumber, taxonID, nlcdClass) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(subplotID = list(if (year <= 2018) subplot_lists$before_2019 else subplot_lists$after_2018)) %>%
    tidyr::unnest(subplotID)
  
  merged <- complete_grid %>%
    dplyr::left_join(
      filtered,
      by = c("siteID", "plotID", "year", "boutNumber", "taxonID", "subplotID", "nlcdClass")
    ) %>%
    dplyr::mutate(percentCover = tidyr::replace_na(percentCover, 0))
  
  per_bout_summary <- merged %>%
    dplyr::group_by(siteID, plotID, year, boutNumber, taxonID, nlcdClass) %>%
    dplyr::summarise(
      total_cover = sum(percentCover),
      .groups = "drop"
    )
  
  # Sum across bouts per plot-year-taxon
  cover_summary <- per_bout_summary %>%
    dplyr::group_by(siteID, plotID, year, taxonID) %>%
    dplyr::summarise(
      total_cover = sum(total_cover),
      .groups = "drop"
    )
  
  # Calculate total cover per plot-year
  total_plot_cover <- cover_summary %>%
    dplyr::group_by(siteID, plotID, year) %>%
    dplyr::summarise(
      plot_total_cover = sum(total_cover),
      .groups = "drop"
    )
  
  # Join and compute relative cover
  relative_cover <- cover_summary %>%
    dplyr::left_join(total_plot_cover, by = c("siteID", "plotID", "year")) %>%
    dplyr::mutate(relative_cover = total_cover / plot_total_cover) %>%
    dplyr::select(siteID, plotID, year, taxonID, relative_cover)
  
  return(relative_cover)
}
