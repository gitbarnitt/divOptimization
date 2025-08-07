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
  
  # Create complete grid per bout
  complete_grid <- filtered %>%
    distinct(siteID, plotID, year, boutNumber, taxonID, nlcdClass) %>%
    rowwise() %>%
    mutate(subplotID = list(if (year <= 2018) subplot_lists$before_2019 else subplot_lists$after_2018)) %>%
    unnest(subplotID)
  
  # Merge and fill in zeros for missing observations
  merged <- complete_grid %>%
    left_join(
      filtered,
      by = c("siteID", "plotID", "year", "boutNumber", "taxonID", "subplotID", "nlcdClass")
    ) %>%
    mutate(percentCover = replace_na(percentCover, 0))
  
  # Step 1: summarize per bout
  per_bout_summary <- merged %>%
    group_by(siteID, plotID, year, boutNumber, taxonID, nlcdClass) %>%
    summarise(
      mean_cover = mean(percentCover),
      sd_cover = sd(percentCover),
      n_subplots = n(),
      .groups = "drop"
    )
  
  # Step 2: average across bouts *only when multiple exist*
  final_summary <- per_bout_summary %>%
    group_by(siteID, plotID, year, taxonID, nlcdClass) %>%
    summarise(
      mean_cover = mean(mean_cover),
      sd_cover = mean(sd_cover),
      n_subplots = sum(n_subplots),
      n_bouts = n_distinct(boutNumber),
      .groups = "drop"
    )
  
  return(final_summary)
}
