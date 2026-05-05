
#check data in input folder
test <- readRDS('C:/Users/dbarnett/Documents/workingGroups/div_optimization_input_plant_data.rds')
data <- readRDS('C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDiversity/v2/project/data/div_optimization_input_plant_data (4).rds')
test4 <- readRDS('C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDiversity/v2/project/data/div_optimization_input_plant_data (4).rds')

#look at results

# Install if needed
#install.packages("arrow")

# Load packages
library(arrow)
library(dplyr)

# Read the files
results <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/20251227_1/div_optimization_output_plant-div-optimization-f4pt2_species_summary.parquet")
data <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/20251227_1/div_optimization_output_plant-div-optimization-f4pt2_relative_cover.parquet")
draws1 <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/DSNY/draws/K10_rep1_2016_2017.parquet")
draws2 <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/HARV/draws/K10_rep1_2016_2017.parquet")
draws3 <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/STER/draws/K10_rep1_2016_2017.parquet")

relCov <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/HARV/div_optimization_outputs_HARV_relative_cover.parquet")
commDec <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/HARV/div_optimization_outputs_HARV_community_detection.parquet")
commDecBase <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/HARV/div_optimization_outputs_HARV_community_detection_baseline.parquet")
drawsInd <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/HARV/div_optimization_outputs_HARV_draws_index.parquet")
spSum <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/HARV/div_optimization_outputs_HARV_species_summary.parquet")
# Quick look at structure
glimpse(results)
head(results)
View(results)  # Opens spreadsheet view in RStudio

# Summary stats
summary(results$mean_detection)

# Which species have high detection (≥80%) with only 10 plots?
results %>%
  filter(sample_size == 10, mean_detection >= 0.80) %>%
  arrange(desc(mean_detection)) %>%
  View()



# Read the files - ornl 20251230
commBaseline <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/ornl/community_detection_baseline.parquet")
commDetection <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/ornl/community_detection.parquet")
drawsIndex <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/ornl/div_optimization_outputs_ORNL_draws_index.parquet")
relativeCover <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/ornl/div_optimization_outputs_ORNL_relative_cover.parquet")
schemaReport <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/ornl/div_optimization_outputs_ORNL_schema_report.parquet")
spSummary <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/Tier1/ornl/div_optimization_outputs_ORNL_species_summary.parquet")


# ---- Suggested plots for community detectability (Tier 1) ----
# Assumes you already have:
#   commDetection <- read_parquet("..._community_detection.parquet")
#   commBaseline  <- read_parquet("..._community_detection_baseline.parquet")
#
# Packages:
library(dplyr)
library(ggplot2)
library(stringr)

# Helper: ensure sample_size is numeric and year_pair ordered
commDetection2 <- commDetection %>%
  mutate(
    sample_size = as.numeric(sample_size),
    requested_sample_size = as.numeric(requested_sample_size),
    year_pair = factor(year_pair, levels = sort(unique(year_pair)))
  )

commBaseline2 <- commBaseline %>%
  mutate(
    sample_size = as.numeric(sample_size),
    requested_sample_size = as.numeric(requested_sample_size),
    year_pair = factor(year_pair, levels = sort(unique(year_pair)))
  )

# 1) Sensitivity curve: cwm_mean vs sample_size with replicate points + mean line by year_pair
p1 <- ggplot(commDetection2, aes(x = sample_size, y = cwm_mean)) +
  geom_hline(yintercept = 0.8) +
  geom_point(aes(group = interaction(year_pair, replicate)), alpha = 0.5) +
  stat_summary(aes(group = year_pair), fun = mean, geom = "line", linewidth = 0.8) +
  facet_wrap(~ year_pair) +
  labs(
    title = "Community-weighted detectability vs sample size (by year pair)",
    x = "Number of plots (sample_size)",
    y = "Community-weighted mean detection probability (cwm_mean)"
  )

print(p1)

# 2) Sensitivity summary: median + 95% interval across replicates, per year_pair and sample_size
commSumm <- commDetection2 %>%
  group_by(site, year_pair, sample_size) %>%
  summarise(
    cwm_med = median(cwm_mean, na.rm = TRUE),
    cwm_lo  = quantile(cwm_mean, 0.025, na.rm = TRUE),
    cwm_hi  = quantile(cwm_mean, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(commSumm, aes(x = sample_size, y = cwm_med)) +
  geom_hline(yintercept = 0.8) +
  geom_line() +
  geom_ribbon(aes(ymin = cwm_lo, ymax = cwm_hi), alpha = 0.2) +
  facet_wrap(~ year_pair) +
  labs(
    title = "Community detectability vs sample size (median across replicates ± 95% interval)",
    x = "Number of plots (sample_size)",
    y = "Median community-weighted detection (cwm_mean)"
  )

print(p2)

# 3) Baseline check: cwm_mean for full available plots, by year_pair, with CI
# (This shows whether the current design clears 0.8 for each year pair.)
p3 <- ggplot(commBaseline2, aes(x = year_pair, y = cwm_mean)) +
  geom_hline(yintercept = 0.8) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15) +
  labs(
    title = "Baseline (full available plots): community-weighted detectability by year pair",
    x = "Year pair",
    y = "Community-weighted mean detection (cwm_mean) with 95% CI"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# 4) Single-panel summary curve across year pairs:
#    show median across year pairs (each year pair contributes one curve point per sample_size)
overallSumm <- commSumm %>%
  group_by(site, sample_size) %>%
  summarise(
    across_pairs_med = median(cwm_med, na.rm = TRUE),
    across_pairs_lo  = quantile(cwm_med, 0.025, na.rm = TRUE),
    across_pairs_hi  = quantile(cwm_med, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

p4 <- ggplot(overallSumm, aes(x = sample_size, y = across_pairs_med)) +
  geom_hline(yintercept = 0.8) +
  geom_line(linewidth = 0.9) +
  geom_ribbon(aes(ymin = across_pairs_lo, ymax = across_pairs_hi), alpha = 0.2) +
  labs(
    title = "Overall community detectability vs sample size (median across year pairs)",
    x = "Number of plots (sample_size)",
    y = "Median community-weighted detection across year pairs"
  )

print(p4)

# ---- Optional: save to files ----
# ggsave("community_detectability_by_yearpair_facets.png", p2, width = 12, height = 8, dpi = 300)
# ggsave("community_detectability_baseline_by_yearpair.png", p3, width = 12, height = 5, dpi = 300)
# ggsave("community_detectability_overall_curve.png", p4, width = 8, height = 5, dpi = 300)


library(arrow)

# Check variable output
cd <- commDec
nrow(cd)  # Total rows
cd %>% 
  distinct(site, year_pair, sample_size, replicate, requested_sample_size, actual_sample_size) %>% 
  nrow()  # Should be same number

# Check baseline output
cd_base <- commDecBase
nrow(cd_base)
cd_base %>% 
  distinct(site, year_pair, sample_size, replicate, requested_sample_size, actual_sample_size) %>% 
  nrow()


# Check variable output
cd <- commDec
nrow(cd)
cd %>% 
  distinct(site, year_pair, sample_size, replicate, requested_sample_size, actual_sample_size) %>% 
  nrow()


library(dplyr)

# Check variable output
commDec %>%
  mutate(mean_outside_ci = cwm_mean < ci_lower | cwm_mean > ci_upper) %>%
  summarize(
    total_rows = n(),
    outside_ci = sum(mean_outside_ci, na.rm = TRUE),
    pct_outside = 100 * outside_ci / total_rows
  )

# Check baseline output
commDecBase %>%
  mutate(mean_outside_ci = cwm_mean < ci_lower | cwm_mean > ci_upper) %>%
  summarize(
    total_rows = n(),
    outside_ci = sum(mean_outside_ci, na.rm = TRUE),
    pct_outside = 100 * outside_ci / total_rows
  )


commDecBase %>% count(year_pair)

commDecBase %>% count(actual_sample_size, sample_size)

commDec %>% group_by(sample_size) %>%
  summarize(
    n_year_pairs = n_distinct(year_pair),
    cwm_mean_med = median(cwm_mean),
    ci_lower_med = median(ci_lower),
    ci_upper_med = median(ci_upper)
  )


library(dplyr)

plots_by_site_year <- data %>%
  group_by(domainID, siteID, year) %>%
  summarise(number_of_plots = dplyr::n_distinct(plotID), .groups = "drop") %>%
  arrange(domainID, siteID, year)

# View the result
print(plots_by_site_year)

# Optional: pretty table for reports
# install.packages("knitr")  # if needed
knitr::kable(plots_by_site_year, align = "l")
``
write.csv(plots_by_site_year, 'C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/report/dataTable.csv')





################powerAnalysisFirstLook
metaJORN <- readRDS('C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/outputs/powerAnalysis/JORN/div_optimization_outputs_JORN_tierp0_metadata.rds')

nStarJORN <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/powerAnalysis/JORN/div_optimization_outputs_JORN_tierp0_n_star_power.parquet")
powerCurveJORN <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/powerAnalysis/JORN/div_optimization_outputs_JORN_tierp0_power_curve.parquet")


metaJORN <- readRDS('C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/outputs/powerAnalysis/JORN/div_optimization_outputs_JORN_tierp0_metadata.rds')

nStarDSNY <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/powerAnalysis/DSNY/div_optimization_outputs_DSNY_tierp0_n_star_power.parquet")
powerCurveDSNY <- read_parquet("C:/Users/dbarnett/OneDrive - Battelle Ecology/optimization/outputs/powerAnalysis/DSNY/div_optimization_outputs_DSNY_tierp0_power_curve.parquet")

nStarJERC <- read_parquet("C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/outputs/tier0/JERC/n_star_power.parquet")
powerCurveJERC <- read_parquet("C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDivOptimizationJob/src/outputs/tier0/JERC/power_curve.parquet")



library(dplyr)

powerCurveJERC %>%
  group_by(sample_size) %>%
  summarise(
    n_species = n(),
    mean_power = mean(power, na.rm = TRUE),
    median_power = median(power, na.rm = TRUE),
    frac_detectable_80 = mean(power >= 0.8, na.rm = TRUE)
  ) %>%
  arrange(sample_size)

nStarJERC %>%
  summarise(
    n_species = n(),
    min_n = min(n_star, na.rm = TRUE),
    median_n = median(n_star, na.rm = TRUE),
    mean_n = mean(n_star, na.rm = TRUE),
    p25 = quantile(n_star, 0.25, na.rm = TRUE),
    p75 = quantile(n_star, 0.75, na.rm = TRUE)
  )

nStarJERC %>%
  count(n_star) %>%
  arrange(n_star)


library(ggplot2)

power_summary <- powerCurveJERC %>%
  group_by(sample_size) %>%
  summarise(
    mean_power = mean(power, na.rm = TRUE),
    frac_detectable_80 = mean(power >= 0.8, na.rm = TRUE)
  )

ggplot(power_summary, aes(x = sample_size)) +
  geom_line(aes(y = frac_detectable_80), linewidth = 1) +
  geom_point(aes(y = frac_detectable_80), size = 2) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "grey40") +
  scale_x_reverse(breaks = sort(unique(power_summary$sample_size))) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Number of plots (N)",
    y = "Fraction of species with power ≥ 0.8",
    title = "Tier 0 power to detect 20% interannual change at JORN",
    subtitle = "Posterior predictive power analysis"
  ) +
  theme_bw()



#####copilot diagnostics

library(arrow)
library(dplyr)

pc <- powerCurveJERC


# How many year-pairs contribute at each K?
cat("=== Year-pair eligibility by sample size ===\n")
pc %>%
  group_by(sample_size) %>%
  summarize(n_year_pairs = n_distinct(paste(year_baseline, year_perturbed))) %>%
  arrange(sample_size) %>%
  print()

# Within a fixed year-pair, is power monotone with K?
cat("\n=== Power trend within each year-pair ===\n")
pc %>%
  group_by(year_baseline, year_perturbed, sample_size) %>%
  summarize(mean_power = mean(power), .groups = 'drop') %>%
  arrange(year_baseline, year_perturbed, sample_size) %>%
  print(n = 100)

# Overall trend (mixing different year-pair sets - misleading!)
cat("\n=== Overall average (MISLEADING - different cases at each K) ===\n")
pc %>%
  group_by(sample_size) %>%
  summarize(mean_power = mean(power)) %>%
  arrange(sample_size) %>%
  print()


library(dplyr)
library(ggplot2)

# Summarize mean and variability across year-pairs for each K
pc_summary <- pc %>%
  group_by(sample_size) %>%
  summarize(
    mean_power = mean(power, na.rm=TRUE),
    sd_power = sd(power, na.rm=TRUE),
    n = n(),
    se_power = sd_power / sqrt(n),
    lower = mean_power - 1.96 * se_power,
    upper = mean_power + 1.96 * se_power
  )

ggplot(pc, aes(x=sample_size, y=power, group=interaction(year_baseline, year_perturbed), color=interaction(year_baseline, year_perturbed))) +
  geom_line(alpha=0.2) + geom_point(alpha=0.4) +
  geom_line(data=pc_summary, aes(x=sample_size, y=mean_power), color="black", size=1.2, inherit.aes=FALSE) +
  geom_ribbon(data=pc_summary, aes(x=sample_size, ymin=lower, ymax=upper), fill="black", alpha=0.2, inherit.aes=FALSE) +
  labs(title="Power Curves for BART", x="Sample Size (K)", y="Detection Probability (Power)", color="Year Pair") +
  theme_minimal()
