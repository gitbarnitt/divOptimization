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

countSp <- site_data %>%
  select(taxonID) %>%
  unique() %>%
  nrow()


test_result <- fit_gjam_model_test(site_data, n_plots = 5)

# Try running the GJAM model manually
fit_result <- fit_gjam_model(site_data)

fit <- fit_gjam_model(site_data)

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

###

manual_fit <- fit_result$fit

x2_df <- site_data %>%
  filter(year == 2021) %>%
  select(year, nlcdClass) %>%
  distinct() %>%
  mutate(
    year = factor(year, levels = levels(manual_fit$xdata$year)),
    nlcdClass = factor(nlcdClass, levels = levels(manual_fit$xdata$nlcdClass))
  )

manual_pred <- tryCatch({
  gjam::gjamPredict(output = manual_fit, newdata = list(xdata = x2_df))
}, error = function(e) {
  message("❌ Prediction failed: ", e$message)
  return(NULL)
})

str(manual_pred)

###

# Print contents of reductList
str(manual_fit$modelList$reductList)

# Print sigma structure (important for REDUCT mode)
str(manual_fit$modelList$sigma)

# Check model type, numFactors, etc.
manual_fit$modelList$typeNames
manual_fit$modelList$reductList$nreduct
manual_fit$modelList$reductList$nn

# Print formula
manual_fit$formula

# Double check input match
str(manual_fit$xdata)
str(x2_df)

###

REDUCT <- manual_fit$modelList$reductList$REDUCT

print(REDUCT)

manual_pred <- tryCatch({
  gjam::gjamPredict(output = manual_fit, newdata = list(xdata = x2_df))
}, error = function(e) {
  message("❌ Prediction still failed: ", e$message)
  return(NULL)
})

str(manual_pred)

###

debugonce(gjam:::`.gjamPrediction`)
manual_pred <- gjam::gjamPredict(output = manual_fit, newdata = list(xdata = x2_df))

###

manual_pred <- with(list(REDUCT = manual_fit$modelList$reductList$REDUCT), {
  tryCatch({
    gjam::gjamPredict(output = manual_fit, newdata = list(xdata = x2_df))
  }, error = function(e) {
    message("❌ Prediction with workaround still failed: ", e$message)
    return(NULL)
  })
})


###

# 1. Copy the original source
gjamPrediction_orig <- getFromNamespace(".gjamPrediction", "gjam")

# 2. Duplicate and patch it
patched_gjamPrediction <- gjamPrediction_orig
environment(patched_gjamPrediction) <- environment()

# 3. Modify the start of the function like this:
body(patched_gjamPrediction)[[2]] <- quote(REDUCT <- output$modelList$reductList$REDUCT)

# 4. Then run:
manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df), 
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# Step 1: Source the original again to reset in case of corruption
gjamPrediction_orig <- getFromNamespace(".gjamPrediction", "gjam")

# Step 2: Copy and patch
patched_gjamPrediction <- gjamPrediction_orig
environment(patched_gjamPrediction) <- environment()
body(patched_gjamPrediction)[[2]] <- quote({
  REDUCT <- output$modelList$reductList$REDUCT
  print(paste("🔍 REDUCT inside patched:", REDUCT))
})

# Step 3: Debug and run it to see where exactly it breaks
debug(patched_gjamPrediction)

# Step 4: Try to call it
manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df),
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Patched prediction failed again: ", e$message)
  return(NULL)
})


###

# 1. Get the original internal function
gjamPrediction_orig <- getFromNamespace(".gjamPrediction", "gjam")

# 2. Make a copy to modify
patched_gjamPrediction <- gjamPrediction_orig
environment(patched_gjamPrediction) <- environment()

# 3. Insert `REDUCT <- output$modelList$reductList$REDUCT` at the beginning of the function body
original_body <- body(patched_gjamPrediction)
injected_line <- quote({
  REDUCT <- output$modelList$reductList$REDUCT
  cat("🔍 Patched REDUCT =", REDUCT, "\n")
})
body(patched_gjamPrediction) <- as.call(c(quote(`{`), injected_line, as.list(original_body)[-1]))

# 4. Enable debugging
debug(patched_gjamPrediction)

# 5. Run and step through
manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df),
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

debug(patched_gjamPrediction)  # sets breakpoint

manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

debugonce(patched_gjamPrediction)
manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df), 
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Still failing: ", e$message)
  return(NULL)
})

###

debugonce(patched_gjamPrediction)
manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df), 
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

patched_gjamPrediction <- gjamPrediction_orig
environment(patched_gjamPrediction) <- environment()

# Patch: Inject REDUCT assignment first
body(patched_gjamPrediction)[[2]] <- quote({
  REDUCT <- output$modelList$reductList$REDUCT
})

# Patch: Fix the if-block to avoid referencing undefined vars
body(patched_gjamPrediction)[[3]] <- quote({
  if (isTRUE(REDUCT)) {
    otherpar <- output$modelList$reductList$otherpar
    N <- otherpar$N
    r <- otherpar$r
    rndEff <- y * 0
    sigmaerror <- otherpar$sigmaerror
    if (!NEWX) 
      SAMEX <- TRUE
    if (COND) 
      stop("conditional prediction not currently implemented with dimension reduction")
  }
})

manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df), 
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###
debug(patched_gjamPrediction)

manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df), 
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Still failing: ", e$message)
  return(NULL)
})

debug(patched_gjamPrediction)  # activate debugging again
manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df), 
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

debug(patched_gjamPrediction)

manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df), 
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})


n
###

patched_gjamPrediction <- gjamPrediction_orig
environment(patched_gjamPrediction) <- environment()

# Patch: insert early assignment and skip all REDUCT logic
body(patched_gjamPrediction)[[2]] <- quote({
  REDUCT <- FALSE  # forcefully disable REDUCT logic
  sigmaerror <- NULL
})

debug(patched_gjamPrediction)

manual_pred <- tryCatch({
  patched_gjamPrediction(output = manual_fit, newdata = list(xdata = x2_df), 
                         y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE)
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# 1. Get original source
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# 2. Copy its environment
environment(patched_gjamPrediction) <- environment()

# 3. Capture the original body as a list of expressions
patched_body <- as.list(body(patched_gjamPrediction))

# 4. Modify the lines:
#   (a) Force REDUCT = FALSE before it's ever used
#   (b) Remove or comment out the if (REDUCT) block entirely
patched_body[[2]] <- quote(REDUCT <- FALSE)
patched_body[[4]] <- quote({})  # replace if (REDUCT) {...} block with empty block

# 5. Rebuild the function body and reassign
body(patched_gjamPrediction) <- as.call(c(quote(`{`), patched_body))

manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})



###

# 1. Get original source
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# 2. Copy its environment
environment(patched_gjamPrediction) <- environment()

# 3. Capture the original body as a list of expressions
patched_body <- as.list(body(patched_gjamPrediction))

# 4. Modify specific lines:
#    (a) Force REDUCT to FALSE up front
#    (b) Replace REDUCT block with diagnostic message
#    (c) Insert debug prints after the REDUCT block
patched_body[[2]] <- quote(REDUCT <- FALSE)

patched_body[[4]] <- quote({
  cat("✅ REDUCT block skipped\n")
})

patched_body[[5]] <- quote({
  cat("🔎 Inspecting objects after REDUCT block...\n")
  cat("🔎 str(x):\n"); print(str(x))
  cat("🔎 str(xnew):\n"); print(str(xnew))
  cat("🔎 str(y):\n"); print(str(y))
})

# 5. Rebuild the function with the modified body
body(patched_gjamPrediction) <- as.call(c(quote(`{`), patched_body))

# 6. Try running the prediction
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})


###

# 1. Get the original function
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# 2. Set its environment so it can use our workspace
environment(patched_gjamPrediction) <- environment()

# 3. Get function body
patched_body <- as.list(body(patched_gjamPrediction))

# 4. Inject patch lines
# (a) Skip REDUCT block
patched_body[[2]] <- quote(REDUCT <- FALSE)
patched_body[[4]] <- quote({ cat("✅ REDUCT block skipped\n") })

# (b) Manually define x (design matrix from training)
patched_body[[5]] <- quote({
  cat("🔧 Defining x from output$xdata\n")
  x <- output$xdata
})

# (c) Inspect x, xnew, y
patched_body[[6]] <- quote({
  cat("🔎 Structure diagnostics:\n")
  cat("str(x):\n"); print(str(x))
  cat("str(xnew):\n"); print(str(xnew))
  cat("str(y):\n"); print(str(y))
})

# 5. Rebuild function
body(patched_gjamPrediction) <- as.call(c(quote(`{`), patched_body))

# 6. Re-run prediction
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# 1. Load original function
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# 2. Set its environment so it can use your workspace
environment(patched_gjamPrediction) <- environment()

# 3. Extract and modify body
patched_body <- as.list(body(patched_gjamPrediction))

# 4. Skip REDUCT
patched_body[[2]] <- quote(REDUCT <- FALSE)
patched_body[[4]] <- quote({ cat("✅ REDUCT block skipped\n") })

# 5. Inject x and xnew definitions manually
patched_body[[5]] <- quote({
  cat("🔧 Defining x and xnew\n")
  x <- output$xdata
  xnew <- newdata$xdata
})

# 6. Optional diagnostics
patched_body[[6]] <- quote({
  cat("🔍 str(x):\n"); print(str(x))
  cat("🔍 str(xnew):\n"); print(str(xnew))
})

# 7. Rebuild the function
body(patched_gjamPrediction) <- as.call(c(quote(`{`), patched_body))

# 8. Run prediction
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})


###

# 1. Load original function
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")
environment(patched_gjamPrediction) <- environment()

# 2. Extract and modify body
patched_body <- as.list(body(patched_gjamPrediction))

# 3. Bypass REDUCT
patched_body[[2]] <- quote(REDUCT <- FALSE)
patched_body[[4]] <- quote({ cat("✅ REDUCT block skipped\n") })

# 4. Define x and xnew
patched_body[[5]] <- quote({
  cat("🔧 Defining x and xnew\n")
  x <- output$xdata
  xnew <- newdata$xdata
  xnew <- as.data.frame(xnew)  # 🔧 critical fix
})

# 5. Diagnostics
patched_body[[6]] <- quote({
  cat("🔍 str(x):\n"); print(str(x))
  cat("🔍 str(xnew):\n"); print(str(xnew))
})

# 6. Rebuild and assign
body(patched_gjamPrediction) <- as.call(c(quote(`{`), patched_body))

# 7. Run prediction
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# Patch again
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")
environment(patched_gjamPrediction) <- environment()
patched_body <- as.list(body(patched_gjamPrediction))

# Skip REDUCT
patched_body[[2]] <- quote(REDUCT <- FALSE)
patched_body[[4]] <- quote({ cat("✅ REDUCT block skipped\n") })

# Define x and xnew
patched_body[[5]] <- quote({
  cat("🔧 Defining x and xnew\n")
  x <- output$xdata
  xnew <- newdata$xdata
  xnew <- as.data.frame(xnew)
})

# Add str diagnostics
patched_body[[6]] <- quote({
  cat("🔍 str(x):\n"); print(str(x))
  cat("🔍 str(xnew):\n"); print(str(xnew))
})

# NEW: Add tracer
patched_body[[7]] <- quote({
  cat("📍 REACHED LINE 7\n")
})

# Rebuild and assign
body(patched_gjamPrediction) <- as.call(c(quote(`{`), patched_body))

# Run it
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# Grab and patch .gjamPrediction again
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")
environment(patched_gjamPrediction) <- environment()
patched_body <- as.list(body(patched_gjamPrediction))

# Skip REDUCT
patched_body[[2]] <- quote(REDUCT <- FALSE)
patched_body[[4]] <- quote({ cat("✅ REDUCT block skipped\n") })

# Define x and xnew
patched_body[[5]] <- quote({
  cat("🔧 Defining x and xnew\n")
  x <- output$xdata
  xnew <- newdata$xdata
  xnew <- as.data.frame(xnew)
})

# Inspect structure
patched_body[[6]] <- quote({
  cat("🔍 str(x):\n"); print(str(x))
  cat("🔍 str(xnew):\n"); print(str(xnew))
})

# Confirm reached
patched_body[[7]] <- quote({
  cat("📍 REACHED LINE 7\n")
})

# 🔧 Patch: define SAMEY
patched_body[[8]] <- quote({
  SAMEY <- FALSE
  cat("✅ SAMEY defined\n")
})

# Rebuild body and assign
body(patched_gjamPrediction) <- as.call(c(quote(`{`), patched_body))

# Run
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# 1. Patch .gjamPrediction again
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")
environment(patched_gjamPrediction) <- environment()
patched_body <- as.list(body(patched_gjamPrediction))

# 2. Insert step-by-step overrides
patched_body[[2]] <- quote(REDUCT <- FALSE)
patched_body[[4]] <- quote({ cat("✅ REDUCT block skipped\n") })
patched_body[[5]] <- quote({
  cat("🔧 Defining x and xnew\n")
  x <- output$xdata
  xnew <- newdata$xdata
  xnew <- as.data.frame(xnew)
})
patched_body[[6]] <- quote({
  cat("🔍 str(x):\n"); print(str(x))
  cat("🔍 str(xnew):\n"); print(str(xnew))
})
patched_body[[7]] <- quote({
  cat("📍 REACHED LINE 7\n")
})
patched_body[[8]] <- quote({
  SAMEY <- FALSE
  cat("✅ SAMEY defined\n")
})
patched_body[[9]] <- quote({
  cat("🔍 Checking y...\n")
  y <- output$y
  if (is.null(y)) stop("❌ y is NULL in output")
  if (nrow(y) == 0) stop("❌ y has zero rows")
  cat("✅ y is valid: ", nrow(y), "rows, ", ncol(y), "cols\n")
})

# 3. Rebuild body
body(patched_gjamPrediction) <- as.call(c(quote(`{`), patched_body))

# 4. Run with tryCatch
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL, PLOT = FALSE, ylim = NULL, FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# 🔧 Restore 'y' to the model object from its inputs
manual_fit$y <- manual_fit$inputs$y

# ✅ Run the patched prediction function
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})


###

debugonce(patched_gjamPrediction)

manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})


###

debugonce(patched_gjamPrediction)

manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})


###

# 🔍 Check if effortMat exists and is valid
cat("🔍 str(output$effortMat):\n")
str(output$effortMat)

# 🔍 Check if rowOrder exists and is valid
cat("\n🔍 str(output$inputs$rowOrder):\n")
str(output$inputs$rowOrder)

# 🔍 Check if effortMat has rows and columns
if (is.null(output$effortMat)) {
  cat("\n❌ output$effortMat is NULL\n")
} else if (length(dim(output$effortMat)) == 0) {
  cat("\n❌ output$effortMat has zero dimensions\n")
} else {
  cat("\n✅ output$effortMat looks okay\n")
}

# 🔍 Check if rowOrder is missing or empty
if (is.null(output$inputs$rowOrder)) {
  cat("❌ output$inputs$rowOrder is NULL\n")
} else if (length(output$inputs$rowOrder) == 0) {
  cat("❌ output$inputs$rowOrder is empty\n")
} else {
  cat("✅ output$inputs$rowOrder looks okay\n")
}

# 👉 Now step forward to trigger the line that likely fails
n


###

debugonce(patched_gjamPrediction)

manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})
 
###

# Get the original function
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# Set environment to current for patching
environment(patched_gjamPrediction) <- environment()

# Convert body to modifiable list of expressions
body_lines <- as.list(body(patched_gjamPrediction))

# Patch 1: Force REDUCT to FALSE
body_lines[[2]] <- quote(REDUCT <- FALSE)

# Patch 2: Remove the original `if (REDUCT)` block (usually at line 4)
body_lines[[4]] <- quote({})  # safely skip

# Patch 3: Insert effortMat guard block after x, xnew, y are defined (around line 8)
safe_effort_block <- quote({
  message("🔧 Checking for effortMat and rowOrder...")
  effortMat <- if (!is.null(output$effortMat)) output$effortMat else NULL
  rowOrder <- if (!is.null(output$inputs$rowOrder)) output$inputs$rowOrder else NULL
  if (!is.null(effortMat) && !is.null(rowOrder)) {
    effort <- effortMat[rowOrder, , drop = FALSE]
  } else {
    message("⚠️ Missing effortMat or rowOrder — using default effort = matrix(1)")
    effort <- matrix(1, nrow = nrow(y), ncol = ncol(y))
  }
})
body_lines <- append(body_lines, list(safe_effort_block), after = 7)

# Rebuild the patched function
body(patched_gjamPrediction) <- as.call(c(quote(`{`), body_lines))


# Restore y into the model
manual_fit$y <- manual_fit$inputs$y

# Run prediction
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# Get the original function
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# Set environment to current for patching
environment(patched_gjamPrediction) <- environment()

# Convert body to modifiable list of expressions
body_lines <- as.list(body(patched_gjamPrediction))

# Patch 1: Force REDUCT to FALSE
body_lines[[2]] <- quote(REDUCT <- FALSE)

# Patch 2: Remove the original `if (REDUCT)` block (usually at line 4)
body_lines[[4]] <- quote({})  # safely skip

# Patch 3: Define y from output (after x and xnew are defined — insert at line 7)
inject_y <- quote(y <- output$y)
body_lines <- append(body_lines, list(inject_y), after = 6)

# Patch 4: Insert effortMat guard block (after line with y)
safe_effort_block <- quote({
  message("🔧 Checking for effortMat and rowOrder...")
  effortMat <- if (!is.null(output$effortMat)) output$effortMat else NULL
  rowOrder <- if (!is.null(output$inputs$rowOrder)) output$inputs$rowOrder else NULL
  if (!is.null(effortMat) && !is.null(rowOrder)) {
    effort <- effortMat[rowOrder, , drop = FALSE]
  } else {
    message("⚠️ Missing effortMat or rowOrder — using default effort = matrix(1)")
    effort <- matrix(1, nrow = nrow(y), ncol = ncol(y))
  }
})
body_lines <- append(body_lines, list(safe_effort_block), after = 7)

# Rebuild the patched function
body(patched_gjamPrediction) <- as.call(c(quote(`{`), body_lines))

# Restore y into the model
manual_fit$y <- manual_fit$inputs$y

manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# Get the original function
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# Set environment to current for patching
environment(patched_gjamPrediction) <- environment()

# Convert body to modifiable list of expressions
body_lines <- as.list(body(patched_gjamPrediction))

# Patch 1: Force REDUCT to FALSE
body_lines[[2]] <- quote(REDUCT <- FALSE)

# Patch 2: Remove the original `if (REDUCT)` block (usually at line 4)
body_lines[[4]] <- quote({})  # safely skip

# Patch 3: Define y from output (after x and xnew are defined — insert at line 7)
inject_y <- quote(y <- output$y)
body_lines <- append(body_lines, list(inject_y), after = 6)

# Patch 4: Insert effortMat guard block (after y)
safe_effort_block <- quote({
  message("🔧 Checking for effortMat and rowOrder...")
  effortMat <- if (!is.null(output$effortMat)) output$effortMat else NULL
  rowOrder <- if (!is.null(output$inputs$rowOrder)) output$inputs$rowOrder else NULL
  if (!is.null(effortMat) && !is.null(rowOrder)) {
    effort <- effortMat[rowOrder, , drop = FALSE]
  } else {
    message("⚠️ Missing effortMat or rowOrder — using default effort = matrix(1)")
    effort <- matrix(1, nrow = nrow(y), ncol = ncol(y))
  }
})
body_lines <- append(body_lines, list(safe_effort_block), after = 7)

# ✅ Patch 5: Insert holdoutIndex guard block
safe_holdout_block <- quote({
  if (!is.null(output$inputs$holdoutIndex)) {
    holdoutIndex <- which(output$inputs$holdoutIndex == 1)
  } else {
    holdoutIndex <- integer(0)
  }
})
body_lines <- append(body_lines, list(safe_holdout_block), after = 8)

# Rebuild the patched function
body(patched_gjamPrediction) <- as.call(c(quote(`{`), body_lines))

# Restore y again (just in case)
manual_fit$y <- manual_fit$inputs$y

# Retry prediction
manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})

###

# Get the original function
patched_gjamPrediction <- getFromNamespace(".gjamPrediction", "gjam")

# Set environment to current for patching
environment(patched_gjamPrediction) <- environment()

# Convert body to modifiable list of expressions
body_lines <- as.list(body(patched_gjamPrediction))

# Patch 1: Force REDUCT to FALSE
body_lines[[2]] <- quote(REDUCT <- FALSE)

# Patch 2: Remove the original `if (REDUCT)` block
body_lines[[4]] <- quote({})

# Patch 3: Define y from output
inject_y <- quote(y <- output$y)
body_lines <- append(body_lines, list(inject_y), after = 6)

# ✅ Patch 6: Ensure x and xnew are explicitly defined
ensure_x_block <- quote({
  if (!exists("x")) x <- output$xdata
  if (!exists("xnew")) xnew <- newdata$xdata
})
body_lines <- append(body_lines, list(ensure_x_block), after = 7)

# Patch 4: Guard for effortMat
safe_effort_block <- quote({
  message("🔧 Checking for effortMat and rowOrder...")
  effortMat <- if (!is.null(output$effortMat)) output$effortMat else NULL
  rowOrder <- if (!is.null(output$inputs$rowOrder)) output$inputs$rowOrder else NULL
  if (!is.null(effortMat) && !is.null(rowOrder)) {
    effort <- effortMat[rowOrder, , drop = FALSE]
  } else {
    message("⚠️ Missing effortMat or rowOrder — using default effort = matrix(1)")
    effort <- matrix(1, nrow = nrow(y), ncol = ncol(y))
  }
})
body_lines <- append(body_lines, list(safe_effort_block), after = 8)

# Patch 5: Guard for holdoutIndex
safe_holdout_block <- quote({
  if (!is.null(output$inputs$holdoutIndex)) {
    holdoutIndex <- which(output$inputs$holdoutIndex == 1)
  } else {
    holdoutIndex <- integer(0)
  }
})
body_lines <- append(body_lines, list(safe_holdout_block), after = 9)

# Patch 7: Check matrix multiplication before solve
linmod_diag <- quote({
  message("🔍 Diagnostic: attempting t(x) %*% x and solve()")
  tryCatch({
    xtx <- t(as.matrix(x)) %*% as.matrix(x)
    tmp <- solve(xtx)
    message("✅ Matrix solve successful")
  }, error = function(e) {
    message("❌ Matrix solve failed: ", e$message)
  })
})
body_lines <- append(body_lines, list(linmod_diag), after = 10)

# Rebuild
body(patched_gjamPrediction) <- as.call(c(quote(`{`), body_lines))


manual_fit$y <- manual_fit$inputs$y

manual_pred <- tryCatch({
  patched_gjamPrediction(
    output = manual_fit,
    newdata = list(xdata = x2_df),
    y2plot = NULL,
    PLOT = FALSE,
    ylim = NULL,
    FULL = FALSE
  )
}, error = function(e) {
  message("❌ Patched prediction failed: ", e$message)
  return(NULL)
})
