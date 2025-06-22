fit_gjam_trend_model <- function(data_subset) {
  xdata <- model.matrix(~ year + nlcdClass, data = data_subset)
  ydata <- data_subset %>%
    select(plotID, taxonID, meanCover) %>%
    pivot_wider(names_from = taxonID, values_from = meanCover, values_fill = 0) %>%
    column_to_rownames("plotID")

  gjam_model <- gjam(formula = as.formula("~ year + nlcdClass"),
                     ydata = ydata, xdata = xdata,
                     modelList = list(ng = 4000, burnin = 1000, typeNames = "CA"))
  return(gjam_model)
}