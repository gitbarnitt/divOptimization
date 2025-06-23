simulate_change <- function(model) {
  y_sim <- model$inputs$y * 1.2
  xdata <- model$inputs$x
  gjam(formula = model$formula, ydata = y_sim, xdata = xdata,
       modelList = list(ng = 2000, burnin = 500, typeNames = 'CA'))
}