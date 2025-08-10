library(dplyr)
library(tidyr)
library(gjam)
library(ggplot2)
source("R/load_neon_data.R")
source("R/compute_relative_cover.R")
#source("R/fit_gjam_model.R")
#source("R/simulate_change.R")
#source("R/simulate_yearly_changes.R")
#source("R/simulate_yearly_changes.R", echo = TRUE)



################# 
#pull in raw data
rawNEONdata <- readRDS('C:/Users/dbarnett/Documents/GitHub/divOptimization/data/plant_data.rds')

#raw <- rawNEONdata


#################
# Load the data and do some filtering for testing
neon_data <- load_neon_data("data/plant_data.rds")

# Subset to a specific site 
site_id <- "JERC"
site_data <- neon_data %>% filter(siteID == site_id)

set.seed(123)
plot_ids <- site_data %>%
  group_by(plotID) %>%
  filter(n_distinct(year) >= 2) %>%
  pull(plotID) %>%
  unique() %>%
  sample(15)

site_data_subset <- site_data %>% filter(plotID %in% plot_ids)


#################
#run gjam
#test_result <- fit_gjam_model_test(site_data)
test_result <- fit_gjam_model_test(site_data_subset)

# Check
table(site_data_subset$year)

# Now run the model
#test_result <- fit_gjam_model_test(site_data_subset, n_plots = 10)

# Confirm year and nlcdClass in fitted model
table(test_result$fit$xdata$year)
table(test_result$fit$xdata$nlcdClass)

#some testing it is working
# Step 1: Fit the model on the subsetted site data
#test_result <- fit_gjam_model_test(site_data)

# Step 2: Extract fit and data components
fit <- test_result$fit
xdata <- test_result$xdata
species <- colnames(test_result$ydata)

# Step 3: Create new covariate matrix (xnew) with constant land cover, different years
ref_row <- xdata[1, ]
xnew <- as.data.frame(ref_row[rep(1, 2), ])

# Explicit levels ensure contrasts are valid
xnew$year <- factor(c("2015", "2016"), levels = c("2015", "2016"))
xnew$nlcdClass <- factor(xnew$nlcdClass, levels = unique(xdata$nlcdClass))

rownames(xnew) <- c("baseline", "year_2016")

# Step 4: Generate posterior predictions
post_preds <- manual_posterior_predict(test_result, xnew)

# Step 5: Pick a species to visualize
target_species <- species[1]  # or manually set e.g., "ACGR2"

# Step 6: Extract predictions to tidy data frame
pred_df <- data.frame(
  value = c(post_preds[, "baseline", target_species],
            post_preds[, "year_2016", target_species]),
  condition = rep(c("2015", "2016"), each = dim(post_preds)[1])
)

# Step 7: Plot using ggplot
library(ggplot2)
ggplot(pred_df, aes(x = value, fill = condition)) +
  geom_density(alpha = 0.6) +
  labs(
    title = paste("Posterior Predictions for", target_species, ": 2015 vs 2016"),
    x = "Predicted Percent Cover",
    y = "Density"
  ) +
  theme_minimal()

#################
#some testing of manual_posterior_predict after alignment with fit_gjam_model_test() 20250804
# ----------------------------------------
# Step 1: Select 2 example rows from fit$xdata
# ----------------------------------------
set.seed(42)
test_xdata <- droplevels(fit$xdata[sample(nrow(fit$xdata), 2), ])

# Make sure 'year' and 'nlcdClass' have correct levels
test_xdata$year <- factor(test_xdata$year, levels = levels(fit$xdata$year))
test_xdata$nlcdClass <- factor(test_xdata$nlcdClass, levels = levels(fit$xdata$nlcdClass))

# ----------------------------------------
# Step 2: Run manual_posterior_predict()
# ----------------------------------------
posterior_array <- manual_posterior_predict(fit, test_xdata)

# ----------------------------------------
# Step 3: Validate shape and contents
# ----------------------------------------

cat("✅ posterior_array dimensions:\n")
print(dim(posterior_array))  # should be: [n_iter, 2 rows, n_species]

stopifnot(length(dim(posterior_array)) == 3)
stopifnot(dim(posterior_array)[2] == 2)  # 2 rows from test_xdata
stopifnot(dim(posterior_array)[1] == nrow(fit$chains$bgibbs))  # posterior draws
stopifnot(dim(posterior_array)[3] == ncol(fit$ydata))  # number of species

# ----------------------------------------
# Step 4: Print example posterior slice
# ----------------------------------------

cat("\n✅ Example posterior for 1 species:\n")
species_index <- sample(1:dim(posterior_array)[3], 1)
draws_for_species <- posterior_array[, , species_index]

print(draws_for_species)  # [iterations, 2 rows]

# ----------------------------------------
# Step 5: Success message
# ----------------------------------------
cat("\n🎉 manual_posterior_predict() ran successfully and returned valid output.\n")





#################
#some testing of simulate change
# 1. Load the simulate_change() function if not already sourced
# source("R/simulate_change.R")

# 2. Run the function using the fitted test model
change_result <- simulate_change(
  fit = test_result,
  change_year = c("2015", "2016"),
  plot_index = 1
)

# 3. Inspect dimensions and names
str(change_result)
# Expect: [n_iter, 2, n_species]
#         dimnames: [[NULL]], ["baseline", "changed"], [species names]

# 4. Choose a species to visualize
target_species <- dimnames(change_result)[[3]][1]  # e.g., "ACGR2"

# 5. Convert to tidy format for ggplot
library(tidyr)
library(dplyr)
library(ggplot2)

pred_df <- data.frame(
  value = c(change_result[, "baseline", target_species],
            change_result[, "changed", target_species]),
  condition = rep(c("2015", "2016"), each = dim(change_result)[1])
)

# 6. Plot posterior draws for one species
ggplot(pred_df, aes(x = value, fill = condition)) +
  geom_density(alpha = 0.6) +
  labs(
    title = paste("Posterior Predictions for", target_species, "in 2015 vs 2016"),
    x = "Predicted Percent Cover",
    y = "Density"
  ) +
  theme_minimal()




#################
#looking at simulate change
change_result <- simulate_change(test_result, change_year = c("2015", "2016"))
summary_df <- summarize_change(change_result)
library(ggplot2)
ggplot(summary_df, aes(x = diff)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = paste("Posterior Change in Cover:", dimnames(change_result)[[3]][1]),
    x = "Change in Predicted Cover (2016 - 2015)",
    y = "Density"
  ) +
  theme_minimal()

####updated simulate change 20250804
# ------------------------------
# SETUP
# ------------------------------

# Choose a subset of plots (e.g., 5 random rows from xdata)
set.seed(123)  # For reproducibility
xdata_subset <- droplevels(fit$xdata[sample(nrow(fit$xdata), 5), ])

# Pick two valid year levels from the model
valid_years <- levels(fit$xdata$year)
change_year <- valid_years[1:2]

# ------------------------------
# RUN FUNCTION
# ------------------------------

# Run the updated simulate_change()
posterior_array <- simulate_change(
  fit = fit,
  change_year = change_year,
  xdata_subset = xdata_subset
)

# ------------------------------
# CHECK OUTPUT
# ------------------------------

# 1. Confirm shape of result
cat("Posterior array dimensions:\n")
print(dim(posterior_array))  # should be: [n_draws, 2, n_species]

# 2. Confirm structure
stopifnot(length(dim(posterior_array)) == 3)
stopifnot(dim(posterior_array)[2] == 2)  # baseline vs changed

# 3. Optional: visualize a species across conditions
# Pick a random species
spp_index <- sample(1:dim(posterior_array)[3], 1)

# Plot the posterior draws for that species under baseline vs changed
hist(posterior_array[, 1, spp_index], main = "Baseline", xlab = "Predicted % Cover", col = "lightblue")
hist(posterior_array[, 2, spp_index], main = "Changed",  xlab = "Predicted % Cover", col = "salmon")

# 4. Optional: inspect difference
diff_vec <- posterior_array[, 2, spp_index] - posterior_array[, 1, spp_index]
cat("Mean change in predicted cover (changed - baseline):", mean(diff_vec), "\n")

# 5. Print summary for confidence
cat("✅ simulate_change() ran successfully.\n")


#################
#testing calculate_detection_probability
# Fit model and generate posterior predictions
#already one this:
#test_result <- fit_gjam_model_test(site_data_subset)
#slight variation on this:
# post_array <- simulate_change(test_result, change_year = c("2015", "2016"), plot_index = 1)
# 
# detection_summary <- calculate_detection_probability(
#   post_array   = post_array,
#   site_id      = test_result$site,
#   sample_size  = length(unique(site_data_subset$plotID)),  # e.g., 15
#   change_year  = c("baseline", "changed")
# )

#later
change_result <- simulate_change(
  fit = test_result,
  change_year  = c("2015", "2016"),
  plot_index   = 1
)

summary_df <- calculate_detection_probability(
  posterior_array = change_result,
  site_id         = "JERC",
  sample_size     = 15,
  year_pair       = c("2015", "2016")
)

#checking on errors from ab


#################
#test loop_simulate_changes()
# test_result is the output of fit_gjam_model_test()
posterior_list <- loop_simulate_changes(test_result)
str(posterior_list)

#this is temporary - does not work
sample_size = length(unique(test_result$fit$xdata$plotID))

summary_list <- purrr::imap(posterior_list, ~ 
                              calculate_detection_probability(
                                posterior_preds = .x,
                                year_pair = strsplit(.y, "_")[[1]],
                                site_id = test_result$site,
                                sample_size = length(unique(test_result$xdata$plotID))
                              )
)
summary_df <- dplyr::bind_rows(summary_list)



posterior_preds <- manual_posterior_predict(test_result, xnew)
str(posterior_preds)


simChange <- simulate_change(test_result, change_year = c("2015", "2016"))

########
summary_list <- purrr::imap(posterior_list, ~ 
                              calculate_detection_probability(
                                posterior_preds = .x,
                                year_pair = strsplit(.y, "_")[[1]],
                                site_id = test_result$site,
                                sample_size = length(unique(test_result$fit$xdata$plotID))
                              )
)

summary_df <- dplyr::bind_rows(summary_list)

dplyr::glimpse(summary_df)
# or
head(summary_df, 10)

library(ggplot2)

ggplot(summary_df, aes(x = year_changed, y = detect_prob, color = species)) +
  geom_line(aes(group = species), alpha = 0.4) +
  labs(title = "Detection Probability by Species and Year Pair",
       y = "Detection Probability", x = "Changed Year") +
  theme_minimal()

##20250805
# Load required libraries
library(purrr)

# Set number of plots to use for simulation
n_plots <- 5  # or 10 or whatever works for your site

# Run loop_simulate_changes using the fitted GJAM object
posterior_list <- loop_simulate_changes(
  fit     = test_result$fit,
  n_plots = n_plots
)

#check structure result 20250805
# List names = year pairs
names(posterior_list)

# Check structure of the first prediction array
str(posterior_list[[1]])

# Dimensions of each posterior array
purrr::map(posterior_list, ~ dim(.x))

# Species names in dimnames
dimnames(posterior_list[[1]])[[3]][1:10]  # First 10 species

library(ggplot2)

# Pick a year pair and species
year_pair <- names(posterior_list)[1]  # e.g. "2014_2015"
pred_array <- posterior_list[[year_pair]]
species_name <- dimnames(pred_array)[[3]][1]  # First species

# Build tibble of posterior draws
posterior_df <- tibble::tibble(
  baseline = pred_array[, "baseline", species_name],
  changed  = pred_array[, "changed",  species_name]
) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "condition", values_to = "prediction")

# Plot
ggplot(posterior_df, aes(x = prediction, fill = condition)) +
  geom_density(alpha = 0.6) +
  labs(title = glue::glue("Posterior predictions for {species_name} ({year_pair})"),
       x = "Predicted cover", y = "Density") +
  theme_minimal()

#jump into calc_detection_prob()
# Run detection probability on one year pair
diff_stats <- calculate_detection_probability(
  posterior_preds = pred_array,
  year_pair       = strsplit(year_pair, "_")[[1]],
  site_id         = test_result$site,
  sample_size     = 5
)

print(diff_stats)

#loop over years
full_detection_summary <- purrr::imap_dfr(
  posterior_list,
  function(pred_array, label) {
    calculate_detection_probability(
      posterior_preds = pred_array,
      year_pair       = strsplit(label, "_")[[1]],
      site_id         = test_result$site,
      sample_size     = 5
    )
  }
)

library(dplyr)
top_species <- full_detection_summary %>%
  filter(detect_prob > 0.8 | detect_prob < 0.2) %>%
  arrange(desc(abs(mean_diff))) %>%
  slice_head(n = 10)

print(top_species)


######
#testing run_sample_size_sensitivity() 20250805

# Example: test with small values
sensitivity_results <- run_sample_size_sensitivity(
  fit_result    = test_result,
  sample_sizes  = c(5, 10),
  n_replicates  = 2,
  seed          = 42
)

library(dplyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

# View dimensions
cat("✅ Rows:", nrow(sensitivity_results), "\n")
cat("✅ Columns:", ncol(sensitivity_results), "\n")
print(colnames(sensitivity_results))

# 1. Summary by sample size and year pair
summary_by_group <- sensitivity_results %>%
  dplyr::group_by(sample_size, year_baseline, year_changed) %>%
  dplyr::summarise(
    n_species = n(),
    n_high_detection = sum(detect_prob > 0.8, na.rm = TRUE),
    n_low_detection  = sum(detect_prob < 0.2, na.rm = TRUE),
    mean_detect_prob = mean(detect_prob, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(sample_size, year_baseline)

print(summary_by_group)

# 2. Top species by magnitude of change
top_species <- sensitivity_results %>%
  filter(detect_prob > 0.8 | detect_prob < 0.2) %>%
  arrange(desc(abs(mean_diff))) %>%
  slice_head(n = 10)

print(top_species)

# 3. Distribution of detect_prob by sample size
ggplot(sensitivity_results, aes(x = detect_prob, fill = factor(sample_size))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(
    title = "Detection Probability by Sample Size",
    x = "Detection Probability",
    y = "Count",
    fill = "Sample Size"
  ) +
  theme_minimal()

# 4. Optional: check replicate success/failure
table(sensitivity_results$fit_status, sensitivity_results$sample_size)

          

######
#test relative cover
# Load the function (if not already sourced)
# source("path/to/compute_relative_cover.R")

# Path to the test NEON raw data (you may already have this in memory or targets)
test_path <- "data/plant_data.rds"

# Run the function
relative_cover_df <- compute_relative_cover(test_path)

# Preview the output
cat("✅ Output dimensions:\n")
cat("Rows:", nrow(relative_cover_df), "\n")
cat("Columns:", ncol(relative_cover_df), "\n")
print(colnames(relative_cover_df))

# Check for duplicates — there should be only one row per siteID-plotID-year-taxonID
duplicate_rows <- relative_cover_df %>%
  dplyr::count(siteID, plotID, year, taxonID) %>%
  dplyr::filter(n > 1)

if (nrow(duplicate_rows) == 0) {
  cat("✅ No duplicate rows per site-plot-year-taxon.\n")
} else {
  cat("❌ Warning: Duplicates found!\n")
  print(duplicate_rows)
}

# Spot-check sum of relative cover per siteID-plotID-year = 1
check_sums <- relative_cover_df %>%
  dplyr::group_by(siteID, plotID, year) %>%
  dplyr::summarise(sum_rel_cover = sum(relative_cover), .groups = "drop")

summary(check_sums$sum_rel_cover)

# Visual check: Histogram of summed relative cover
ggplot(check_sums, aes(x = sum_rel_cover)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Relative Cover Sums per Plot-Year",
       x = "Sum of Relative Cover", y = "Count") +
  theme_minimal()


######
#evaluate_community_weighted_detection
community_detection <- evaluate_community_weighted_detection(
  sensitivity_results = sensitivity_results,
  relative_cover_df   = relative_cover_df
)

# Preview results
print(head(community_detection))

community_detection <- evaluate_community_weighted_detection(
  sensitivity_results = sensitivity_results,      # updated!
  relative_cover_df   = relative_cover_df
)
head(community_detection)


######
#testing run_sample_size_sensitivity()

sensitivity_results <- run_sample_size_sensitivity(test_result)

# Extract the plot IDs used in each replicate
library(dplyr)

fit_xdata <- test_result$fit$xdata

sample_size_to_check <- 10  # Change as needed
site_to_check <- unique(test_result$site)

# We'll extract sampled plots from xdata used by the model
replicate_plot_sets <- lapply(seq_len(3), function(rep_num) {
  set.seed(123 + sample_size_to_check * 100 + rep_num)  # must match updated function
  sample(unique(fit_xdata$plotID), sample_size_to_check)
})

# Turn into a data frame for easy comparison
plot_sets_df <- tibble::tibble(
  replicate = 1:3,
  plots = replicate_plot_sets
)

print("✅ Sampled plot IDs per replicate:")
print(plot_sets_df)

# Optional: See how much they overlap
combn(1:3, 2, simplify = FALSE) %>%
  purrr::walk(~ {
    i <- .x[1]; j <- .x[2]
    overlap <- length(intersect(replicate_plot_sets[[i]], replicate_plot_sets[[j]]))
    cat(glue::glue("Replicate {i} vs {j}: {overlap} overlapping plots\n"))
  })


######
library(dplyr)
library(tidyr)

# Rebuild what the function does
results_with_pairs <- sensitivity_results %>%
  mutate(year_pair = paste0(year_baseline, "_", year_changed)) %>%
  filter(!is.na(plot_ids))

cover_renamed <- relative_cover_df %>%
  rename(site_cover = siteID, species_cover = taxonID)

# Reproduce the join + unnest step exactly
test_join <- results_with_pairs %>%
  rowwise() %>%
  mutate(rel_cover = list(
    cover_renamed %>%
      filter(site_cover == site,
             species_cover == species,
             year %in% c(as.integer(year_baseline), as.integer(year_changed)),
             plotID %in% plot_ids)
  )) %>%
  ungroup() %>%
  unnest(rel_cover, names_sep = "_rel")

names(test_join)

######
#updated with uncertainty 20250807
sensitivity_results <- run_sample_size_sensitivity(
  fit_result    = test_result,
  sample_sizes  = c(5, 10),
  n_replicates  = 3,  # or whatever number you'd like
  seed          = 123
)


#extra
summary_only <- purrr::map_dfr(sensitivity_results, "summary")

full_detection_summary <- summarize_species_detection_with_uncertainty(
  summary_df = sensitivity_results$summary,
  draws_df   = sensitivity_results$draws
)

# (2) Community-weighted mean with credible intervals
community_detection <- evaluate_community_weighted_detection(
  sensitivity_results = sensitivity_results$summary,
  relative_cover_df   = relative_cover_df,
  draws_df            = sensitivity_results$draws
)


#summary work
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)

# Helper: detect if CI columns are present
has_ci <- all(c("ci_lower", "ci_upper") %in% names(community_detection))

# Average across replicates for each site × sample_size × year_pair
community_rep_avg <- community_detection %>%
  mutate(year_pair = ifelse(is.null(year_pair),
                            paste0(year_baseline, "_", year_changed), year_pair)) %>%
  group_by(site, sample_size, year_pair) %>%
  summarise(
    mean_detection = mean(weighted_detection, na.rm = TRUE),
    ci_lower = if (has_ci) mean(ci_lower, na.rm = TRUE) else NA_real_,
    ci_upper = if (has_ci) mean(ci_upper, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )





# # Check structure of result
# str(posterior_list, max.level = 1)
# 
# # Optionally, peek at one of the year pairs
# example_pair <- names(posterior_list)[1]
# example_array <- posterior_list[[example_pair]]
# 
# dim(example_array)  # Should be [iterations, 2, species]






########
#sensitivity test
# Quick test: 2 sample sizes, 2 replicates each
sensitivity_test <- run_sample_size_sensitivity(
  full_site_data = site_data,
  site_id        = site_id,
  sample_sizes   = c(5, 10),   # keep small for test
  n_replicates   = 2,
  seed           = 42
)





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


#test_result <- fit_gjam_model_test(site_data, n_plots = 5)

test_result <- fit_gjam_model_test(site_data)

fit <- test_result

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
