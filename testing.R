library(dplyr)
library(tidyr)
library(gjam)
source("R/load_neon_data.R")
source("R/fit_gjam_model.R")
#source("R/simulate_change.R")
source("R/simulate_yearly_changes.R")

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
fit <- fit_gjam_model(site_data)

#try running new simulate change function
result <- simulate_yearly_changes(fit)

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

