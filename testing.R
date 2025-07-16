library(dplyr)
library(tidyr)
library(gjam)
source("R/load_neon_data.R")
source("R/fit_gjam_model.R")
#source("R/simulate_change.R")
source("R/simulate_yearly_changes.R")
#source("R/simulate_yearly_changes.R", echo = TRUE)

# pull in raw data
#rawNEONdata <- readRDS('C:/Users/dbarnett/Documents/GitHub/divOptimization/data/plant_data.rds')

#raw <- rawNEONdata

# Load the full dataset
neon_data <- load_neon_data("data/plant_data.rds")

# Subset to a specific site with known issues
site_id <- "JERC"
site_data <- neon_data %>% filter(siteID == site_id)

# Force mean_cover to numeric prior to pivot_wider
#site_data <- site_data %>%
#  mutate(mean_cover = as.numeric(mean_cover))

# Try running the GJAM model manually
fit_result <- fit_gjam_model(site_data)

table(fit_result$xdata$year, useNA = "always")

#try running new simulate change function
result <- simulate_yearly_changes(fit_result)

#check diagnostics
fit$fit$xdata$year
table(fit$fit$xdata$year)

#how many years do we have?
unique()

#checking stuff
checkSRER <- rawNEONdata %>%
  filter(year == 2017) %>%
  filter(plotID == "SRER_029")

checkSRER2 <- complete_grid %>%
  filter(year == 2017) %>%
  filter(plotID == "SRER_029")

checkSRER3 <- merged %>%
  filter(year == 2017) %>%
  filter(plotID == "SRER_029") %>%
  filter(taxonID == "ERLE")

checkJERC <- rawNEONdata %>%
  filter(year == 2016) %>%
  filter(plotID == "JERC_013")

checkJERC2 <- merged %>%
  filter(year == 2016) %>%
  filter(plotID == "JERC_013") %>%
  filter(taxonID == "ANVI2")

##verify

# Pick a failed year pair to investigate
year1 <- "2022"
year2 <- "2023"

# Extract x2: predictor rows for year2
x2 <- fit_result$xdata[fit_result$xdata$year == year2, , drop = FALSE]

# Check factor levels alignment
cat("Levels in model$xdata$year:\n")
print(levels(fit_result$fit$xdata$year))

cat("Levels in x2$year:\n")
print(levels(x2$year))

# Ensure year and nlcdClass factors have matching levels
x2$year <- factor(x2$year, levels = levels(fit_result$fit$xdata$year))
x2$nlcdClass <- factor(x2$nlcdClass, levels = levels(fit_result$fit$xdata$nlcdClass))

# Reorder columns to match model$xdata
x2 <- x2[, names(fit_result$fit$xdata), drop = FALSE]

# Try predicting manually
pred_base <- tryCatch({
  gjam::gjamPredict(output = fit_result$fit, newdata = list(xdata = x2))
}, error = function(e) {
  message("❌ Manual prediction failed: ", e$message)
  return(NULL)
})

cat("Original design matrix (fit$xdata):\n")
mm_model <- model.matrix(fit_result$fit$formula, data = fit_result$fit$xdata)
print(dim(mm_model))
print(colnames(mm_model))

cat("\nDesign matrix for x2:\n")
mm_x2 <- model.matrix(fit_result$fit$formula, data = x2)
print(dim(mm_x2))
print(colnames(mm_x2))

cat("\n🔍 Top-level components in fit_result$fit:\n")
print(names(fit_result$fit))

cat("\n🔍 Does it have a 'parameters' element?\n")
print("parameters" %in% names(fit_result$fit))

cat("\n🔍 Class of fit_result$fit:\n")
print(class(fit_result$fit))

###
cat("\n🔍 Structure of modelList inside fit:\n")
str(fit_result$fit$modelList)

cat("\n🔍 Structure of inputs inside fit:\n")
str(fit_result$fit$inputs)

print(gjam::gjamPredict)

###

print(fit_result$fit$typeNames)

###

fit_result$fit$typeNames <- fit_result$fit$modelList$typeNames

pred_base <- tryCatch({
  gjam::gjamPredict(output = fit_result$fit, newdata = list(xdata = x2))
}, error = function(e) {
  message("❌ Manual prediction failed: ", e$message)
  return(NULL)
})

###

str(fit_result$fit, max.level = 1)
str(fit_result$fit$modelList)
str(fit_result$fit$parameters)

pred_base <- tryCatch({
  gjam::gjamPredict(output = fit_result$fit, newdata = list(xdata = x2), FULL = TRUE)
}, error = function(e) {
  message("❌ FULL prediction failed: ", e$message)
  return(NULL)
})

pred_base <- tryCatch({
  gjam::gjamPredict(output = fit_result$fit, newdata = list(xdata = x2), y2plot = NULL)
}, error = function(e) {
  message("❌ Prediction with y2plot=NULL failed: ", e$message)
  return(NULL)
})

print(typeof(fit_result$fit))
print(class(fit_result$fit))

###

fit_result$fit$ydata <- fit_result$fit$inputs$ydata

pred_base <- tryCatch({
  gjam::gjamPredict(output = fit_result$fit, newdata = list(xdata = x2))
}, error = function(e) {
  message("❌ Prediction failed after ydata patch: ", e$message)
  return(NULL)
})

###

gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

manual_pred <- tryCatch({
  gjamPrediction(output = fit_result$fit, newdata = list(xdata = x2), y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Direct .gjamPrediction failed: ", e$message)
  return(NULL)
})

###

gjamPrediction(
  output = fit_result$fit,
  newdata = list(xdata = x2),
  y2plot = NULL,
  PLOT = FALSE,
  ylim = NULL,
  FULL = FALSE
)

###

fit_result$fit$modelList$reductList <- list(REDUCT = FALSE)

gjam::gjamPredict(output = fit_result$fit, newdata = list(xdata = x2))

###

fit_result$fit$reductList <- list(REDUCT = FALSE)

gjam::gjamPredict(output = fit_result$fit, newdata = list(xdata = x2))

###

fit_result$fit$modelList$reductList <- list(REDUCT = FALSE)

print(str(fit_result$fit$modelList$reductList))

###
# optional cleanup
rm(pred_base, manual_pred)

# final check
print(fit_result$fit$modelList$reductList$REDUCT)  # should print [1] FALSE

# now run prediction
pred_base <- tryCatch({
  gjam::gjamPredict(output = fit_result$fit, newdata = list(xdata = x2))
}, error = function(e) {
  message("❌ Prediction still failed: ", e$message)
  return(NULL)
})

### - did not do this yet

gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# Copy function into global environment
patchedPrediction <- gjamPrediction

# Use trace to inspect and override REDUCT assignment
trace("patchedPrediction", edit = TRUE)

###
# Assuming xdata and ydata from fit_result$fit$inputs
manual_fit <- gjam::gjam(
  formula = fit_result$fit$formula,
  xdata   = fit_result$fit$inputs$xdata,
  ydata   = fit_result$fit$inputs$ydata,
  modelList = list(
    typeNames = fit_result$fit$modelList$typeNames
  )
)

# Then predict
manual_pred <- tryCatch({
  gjam::gjamPredict(output = manual_fit, newdata = list(xdata = x2))
}, error = function(e) {
  message("❌ Manual test failed: ", e$message)
  return(NULL)
})

###

# Structure of x2
str(x2)

# Is x2 a tibble? Convert to plain data.frame
x2_df <- as.data.frame(x2)

# Are there any missing levels? Force factors to match training set
x2_df$year <- factor(x2_df$year, levels = levels(fit_result$fit$xdata$year))
x2_df$nlcdClass <- factor(x2_df$nlcdClass, levels = levels(fit_result$fit$xdata$nlcdClass))

# Try again with data.frame instead of tibble
manual_pred2 <- tryCatch({
  gjam::gjamPredict(output = manual_fit, newdata = list(xdata = x2_df))
}, error = function(e) {
  message("❌ Manual test with data.frame failed: ", e$message)
  return(NULL)
})


###

debugonce(gjam:::gjamPredict)  # not necessary, but helps visibility
debugonce(gjam:::`.gjamPrediction`)
gjam::gjamPredict(output = manual_fit, newdata = list(xdata = x2_df))

n

