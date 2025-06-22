fit_gjam_model <- function(site_data) {
  y <- site_data %>%
    pivot_wider(names_from = taxonID, values_from = mean_cover, values_fill = 0) %>%
    select(-siteID, -plotID, -year, -nlcdClass)
  x <- site_data %>%
    distinct(plotID, year, nlcdClass) %>%
    mutate(year = as.numeric(scale(year))) %>%
    column_to_rownames("plotID")

  formula <- as.formula(paste("~", paste(colnames(x), collapse = " + ")))

  gjam(formula = formula, ydata = y, xdata = x,
       modelList = list(ng = 2000, burnin = 500, typeNames = 'CA'))
}