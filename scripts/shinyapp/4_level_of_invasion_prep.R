#'This script loads two csv-files (occupancy_RBU.csv and occupancy_RBSU.csv) 
#'and adds extra columns two the files describing the level of invasion per species per area per state
#'Four output files are generated:
#'occupancy_RBU.csv: extra columns describe level of invasion per area and relative occupancy per species per river basin unit per state (baseline or current) 
#'header observations_RBU.csv: scientific_name,"RBU","state", "n_observations","Occupancy", "Total_cellcode_per_area", "Occupancy_rel", "level_of_invasion"
#'occupancy_RBSU.csv: extra columns describe level of invasion per area and relative occupancy  per species per river basin subunit per state (baseline or current)
#'header observations_RBSU.csv: scientific_name,"A0_CODE","state","n_observations", "Occupancy", "Total_cellcode_per_area", Occupancy_rel", "level_of_invasion"
#'level_of_invasion_RBU.geojson: Per species (column_names) and state (separate column) level of invasion (values) per RBU (rownames) are defined
#'level_of_invasion_RBSU.geojson Per species (column_names) and state (separate column) level of invasion (values) per RBSU (rownames) are defined

#'@param branch A string referring to the branch on github on which input data is read in
#'@param RBU geojson file in WGS 84 projection containing polygons at river basin unit level
#'@param RBSU geojson file in WGS 84 projection containing polygons at river basin subunit level
#'@param EEA_1km

#!REMARK!: manually remove files "./data/interim/level_of_invasion_RBU.geojson" and "./data/interim/level_of_invasion_RBSU.geojson" before running this script.

#import libraries####
library(leaflet)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)

#import data####
branch <- "41_extending_baseline_map"

##import geojson of RBU and RBSU####
RBU <- st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/",
                               branch,
                               "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE))
RBSU <- st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/",
                                branch,
                                "/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"), stringsAsFactors = FALSE))

##import relative occupancy####
occupancy_RBU <- read.csv(paste0("./data/interim/occupancy_RBU.csv"))
occupancy_RBSU <- read.csv(paste0("./data/interim/occupancy_RBSU.csv"))


 
#define level of invasion depending on relative occupancy####
occupancy_RBU <- occupancy_RBU %>%
  mutate(
  level_of_invasion = case_when(
    Occupancy_rel <= 0.10 ~ "scattered occurences only",
    Occupancy_rel <= 0.30 ~ "weakly invaded",
    Occupancy_rel > 0.30 ~ "heavily invaded")
  )
   
occupancy_RBSU <- occupancy_RBSU %>%
  mutate(
    level_of_invasion = case_when(
      Occupancy_rel <= 0.10 ~ "scattered occurences only",
      Occupancy_rel <= 0.30 ~ "weakly invaded",
      Occupancy_rel > 0.30 ~ "heavily invaded")
  )

#add level of invasion (as value) per species (as column names) to spatial RBSU layer####
occupancy_RBSU_temp<- occupancy_RBSU %>% select(c('A0_CODE', 'scientific_name', 'state', 'level_of_invasion'))
occupancy_RBSU_wider <- occupancy_RBSU_temp %>% pivot_wider(names_from= "scientific_name", values_from= "level_of_invasion")

level_of_invasion_RBSU <- merge(RBSU, occupancy_RBSU_wider, by='A0_CODE')

occupancy_RBU_temp<- occupancy_RBU %>% select(c('RBU', 'scientific_name', 'state', 'level_of_invasion'))
occupancy_RBU_wider <- occupancy_RBU_temp %>% pivot_wider(names_from= "scientific_name", values_from= "level_of_invasion")

level_of_invasion_RBU <- merge(RBU, occupancy_RBU_wider, by.x='BEKNAAM', by.y="RBU")

#export output
st_write(level_of_invasion_RBSU,"./data/interim/level_of_invasion_RBSU.geojson", append=FALSE)
st_write(level_of_invasion_RBU, "./data/interim/level_of_invasion_RBU.geojson", append=FALSE)

#write.csv(occupancy_RBU, './data/interim/occupancy_RBU.csv', row.names=FALSE)
#write.csv(occupancy_RBSU, './data/interim/occupancy_RBSU.csv', row.names=FALSE)


