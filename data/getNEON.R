# load neonUtilities 
library (neonUtilities)
library(dplyr)

#specify data product identification
presDataProductID <- as.character('DP1.10058.001')

sites <- "OSBS"

#get data, all sites
allDiv <- loadByProduct(dpID = presDataProductID, 
                        #startdate = startDate, 
                        #enddate = endDate,                       
                        site = sites,
                        #site = 'all',
                        package = 'expanded', 
                        release = "current",
                        check.size = FALSE,
                        token = Sys.getenv('NEON_API')) # remove this line if you don't have a token, though everyone is encouraged to get one

# Turn all tables in the list to dataframe (DF) in the global environment, where name of table = name of DF. This is helpful because most functions require a DF as input. The list itself is still preserved when this command is run, for those few functions that actually need lists.
list2env(allDiv, envir=.GlobalEnv)

###fix remaining errant eventID
#fix those that are na for some reason still
div_1m2Data <- div_1m2Data %>%
  mutate(year = substr(endDate, start = 1, stop = 4)) %>%
  mutate(eventID = case_when(
    is.na(eventID) ~ paste(siteID, ".", boutNumber, ".", year, sep = ""),
    TRUE ~ as.character(eventID)))

#keep year for matching field 
#div_1m2Data <- select(div_1m2Data, -year)

#fix those that still include plotID for some reason
div_1m2Data <- div_1m2Data %>%
  mutate(eventID = case_when(
    nchar(eventID) == 15 ~ paste(substr(eventID, start = 1, stop = 4),substr(eventID, start = 9, stop = 15), sep = ""),
    TRUE ~ as.character(eventID)
  ))

###apply morphospecies corrections
#set morph table to date of interest and remove duplicates
div_morphospecies <-  div_morphospecies %>%
  select(siteID, morphospeciesID, taxonID, scientificName, identificationQualifier) %>% 
  unique()

#join morph table to 1m2 data
div_1m2Data <- div_1m2Data %>%
  left_join(select(div_morphospecies, siteID, morphospeciesID, taxonID, scientificName, identificationQualifier), by = c('siteID' = 'siteID', 'morphospeciesID' = 'morphospeciesID')) 

div_1m2Data <- div_1m2Data %>%
  mutate(taxonID = ifelse(!is.na(div_1m2Data$taxonID.y), div_1m2Data$taxonID.y, div_1m2Data$taxonID.x)) %>%
  mutate(scientificName = ifelse(!is.na(div_1m2Data$scientificName.y), div_1m2Data$scientificName.y, div_1m2Data$scientificName.x)) %>%
  mutate(identificationQualifier = ifelse(!is.na(div_1m2Data$identificationQualifier.y), div_1m2Data$identificationQualifier.y, div_1m2Data$identificationQualifier.x)) %>%
  select(-taxonID.y, -taxonID.x, -scientificName.x, -scientificName.y, -identificationQualifier.x, -identificationQualifier.y) 

###limit 1m2 data to plant species records and samplingImprac by removing otherVars
div_1m2Data <- filter(div_1m2Data, divDataType != "otherVariables")

###add year
div_1m2Data$year <- substr(div_1m2Data$eventID, start = 8, stop = 11)


###get rid of extra fields that could be hard for generating unique records
#data_1m2
div_1m2Data <- dplyr::select(div_1m2Data, domainID,	siteID,	year, nlcdClass, eventID, plotType, boutNumber, plotID,	subplotID, taxonID,	percentCover, targetTaxaPresent, samplingImpractical) 

###remove towerPlots
div_1m2Data <- filter(div_1m2Data, plotType == 'distributed') %>%
  select(-plotType)

#get only 1 bout per site for these purposes
#div_1m2Data <- div_1m2Data %>%
#  filter(!(siteID %in% c("OAES", "JORN", "SRER") & boutNumber == 1))

#dump some other bout 2 records
#div_1m2Data <- div_1m2Data %>%
#  filter(!(siteID %in% c("CPER", "WOOD") & boutNumber ==2))

#GRSM kinda tricky
#div_1m2Data <- div_1m2Data %>%
#  filter(siteID != "GRSM" |  # Keep all records where siteID is not "GRSM"
#           (siteID == "GRSM" &
#              ((year == 2015 & boutNumber == 1) |
#                 (year %in% 2016:2019 & boutNumber == 2) |
#                 (year >= 2020 & boutNumber == 1))))

str(div_1m2Data)

saveRDS(div_1m2Data, 'C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDiversity/v2/code/neon-gjam-targets/data/plant_data.rds')


subsetData <- readRDS('C:/Users/dbarnett/Documents/GitHub/NEON-OS-optimization/plantDiversity/v2/project/data/plant_data.rds')

subsetData <- subsetData %>%
  filter(siteID == "OSBS", year %in% c(2022, 2023, 2024)) %>%
  distinct(plotID) %>%
  slice_head(n = 5) %>%
  inner_join(subsetData, by = "plotID")

saveRDS(subsetData, 'C:/Users/dbarnett/Documents/GitHub/divOptimization/data/plant_data.rds')


