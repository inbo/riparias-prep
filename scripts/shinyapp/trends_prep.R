
# Setup ####
## Libraries ####
library(tidyverse) # To do data science
library(tidylog) # To provide feedback on dplyr functions
library(progress) # To add progress bars
library(here) # To find files
library(lubridate) # To work with dates
library(rgdal)
library(rgbif)
library(trias)

# Read data ####
points_in_perimeter <- readOGR("./data/spatial/baseline/points_in_perimeter.geojson", stringsAsFactors = FALSE)

df <- points_in_perimeter@data %>% 
  rename(taxonKey = accptTK)

# trias prep ####
## classinfo ####
taxon_key <-
  df %>%
  distinct(taxonKey) %>% 
  pull()

spec_names <- data.frame()
for(k in taxon_key){
  spec_names_temp <- name_usage(key = k)
  spec_names_temp <- spec_names_temp$data %>% 
    select(taxonKey = key,
           canonicalName,
           scientificName,
           kingdomKey, 
           kingdom,
           classKey,
           class)%>%
    mutate(canonicalName = ifelse(
      is.na(canonicalName), scientificName, canonicalName
    ))
  if(nrow(spec_names)==0){
    spec_names <- spec_names_temp
  }else{
    spec_names <- rbind(spec_names, spec_names_temp)
  }
}

## baseline ####
## timeseries ####
## observer bias ####
## modelling prep ####

# export ####
