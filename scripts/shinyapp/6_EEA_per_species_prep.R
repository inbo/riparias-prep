rm(list=ls())

#This script returns EEA grid per species in geojson format for both states (baseline and current).
#'@param branch A string referring to the branch on github on which input data is read in
#'@param current_state geosjon file in WGS 84 projection containing observations from 2010 until now
#'@param baseline geojson file in WGS 84 projection containing observations from 2010 to 2020


#'#load libraries####
library(leaflet)
library(rgdal)
library(sf)
library(dplyr)

#read in input####
branch <- "50_add_species"

current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/current_state.geojson"), stringsAsFactors = FALSE)

baseline <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/baseline.geojson"), stringsAsFactors = FALSE)


##import EEA 1km file####
EEA_1km <- readOGR("data/spatial/EEA 1km/be_1km.shp")

##define correct projection####
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

EEA_1km <- spTransform(EEA_1km, crs_wgs)

EEA_1km_sf <- st_as_sf(EEA_1km)

#intersect of baseline occurences with EEA####
gc()
baseline_EEA <- raster::intersect(baseline, EEA_1km)

baseline_EEA_data <- as.data.frame(baseline_EEA@data)

species_EEA_baseline <- baseline_EEA_data %>%
  select(scientific_name, CELLCODE)%>%
  distinct()

EEA_1km_baseline <- merge(EEA_1km_sf, species_EEA_baseline, by="CELLCODE")

st_write(EEA_1km_baseline, "./data/interim/EEA_per_species_baseline.geojson")


#intersect of current occurences with EEA####
gc()
current_EEA <- raster::intersect(current_state, EEA_1km)

current_EEA_data <- as.data.frame(current_EEA@data)

species_EEA_current <- current_EEA_data %>%
  select(scientific_name, CELLCODE)%>%
  distinct()

EEA_1km_current <- merge(EEA_1km_sf, species_EEA_current, by="CELLCODE")

st_write(EEA_1km_current, "./data/interim/EEA_per_species_current.geojson")
