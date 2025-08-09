library(dplyr)

#pull in raw data
rawNEONdata <- readRDS('C:/Users/dbarnett/Documents/GitHub/divOptimization/data/plant_data.rds')

# Subset to a specific site 
site_id <- "JERC"
site_data <- rawNEONdata %>% filter(siteID == site_id)

set.seed(123)
plot_ids <- site_data %>%
  group_by(plotID) %>%
  filter(n_distinct(year) >= 2) %>%
  pull(plotID) %>%
  unique() %>%
  sample(15)

site_data_subset <- site_data %>% filter(plotID %in% plot_ids)

#20250808 test targets
saveRDS(site_data_subset, 'C:/Users/dbarnett/Documents/GitHub/divOptimization/data/plant_data.rds')


