load_neon_data <- function(path) {
  raw <- readRDS(path)

  subplot_lists <- list(
    before_2019 = c("31_1_1", "32_1_4", "32_1_2", "40_1_3", "40_1_1", "41_1_1", "41_1_4", "31_1_4"),
    after_2018 = c("31_1_1", "32_1_4", "32_1_2", "40_1_3", "40_1_1", "41_1_4")
  )

  filtered <- raw %>%
    filter(targetTaxaPresent == "Y") %>%
    mutate(year = as.integer(year)) %>%
    filter(is.na(samplingImpractical) | samplingImpractical == "OK")

  complete_grid <- filtered %>%
    distinct(siteID, plotID, year, taxonID, nlcdClass) %>%
    rowwise() %>%
    mutate(subplotID = list(if (year <= 2018) subplot_lists$before_2019 else subplot_lists$after_2018)) %>%
    unnest(subplotID)

  merged <- complete_grid %>%
    left_join(filtered, by = c("siteID", "plotID", "year", "taxonID", "subplotID", "nlcdClass")) %>%
    mutate(percentCover = replace_na(percentCover, 0))

  summarized <- merged %>%
    group_by(siteID, plotID, year, taxonID, nlcdClass) %>%
    summarise(
      mean_cover = mean(percentCover, na.rm = TRUE),
      sd_cover = sd(percentCover, na.rm = TRUE),
      n_subplots = n(),
      .groups = "drop"
    )

  return(summarized)
}