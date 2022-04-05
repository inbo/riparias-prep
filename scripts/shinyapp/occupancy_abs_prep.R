#'This script loads two csv-files (observations_RBU.csv and observations_RBSU.csv) 
#'and adds extra columns two the files describing the occupancy per species per area per state
#'Two output files are generated:
#'occupancy_RBU.csv: extra columns describe occupancy per species per river basin unit per state (baseline or current) 
#'header observations_RBU.csv: scientific_name,"RBU","n_observations","state", "Occupancy"
#'occupancy_RBSU.csv: extra columns describe occupancy  per species per river basin subunit per state (baseline or current)
#'header observations_RBSU.csv: scientific_name,"A0_CODE","n_observations","state", "Occupancy"
#'
#'@param branch A string referring to the branch on github on which input data is read in
#'@param current_state geosjon file in WGS 84 projection containing observations from 2010 until now
#'@param baseline geojson file in WGS 84 projection containing observations from 2010 to 2020
#'@param RBU geojson file containing polygons at river basin unit level
#'@param RBSU geojson file containing polygons at river basin subunit level

#load libraries####
library(leaflet)
library(rgdal)
library(sf)
library(dplyr)


#read in input####
branch <- "41_extending_baseline_map"

current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/current_state.geojson"), stringsAsFactors = FALSE)

baseline <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/baseline.geojson"), stringsAsFactors = FALSE)

RBU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE)

RBSU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch,"/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"), stringsAsFactors = FALSE)

##read in observations####
observations_RBU <- read.csv("./data/interim/observations_RBU.csv")
observations_RBSU <- read.csv("./data/interim/observations_RBSU.csv")
##import EEA 1km file####
EEA_1km <- readOGR("data/spatial/EEA 1km/be_1km.shp")

##define correct projection####
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

EEA_1km <- spTransform(EEA_1km, crs_wgs)

#intersect of baseline occurences with RBU####
baseline_in_RBU <- raster::intersect(baseline, RBU)
baseline_in_RBU_EEA <- raster::intersect(baseline_in_RBU, EEA_1km)

baseline_in_RBU_EEA_data <- as.data.frame(baseline_in_RBU_EEA@data)

names(baseline_in_RBU_EEA_data)[10] <-'RBU'

baseline_per_RBU_EEA <- baseline_in_RBU_EEA_data %>%
  select(scientific_name, RBU, CELLCODE)%>% 
  group_by (scientific_name, RBU)%>%
  mutate(Occupancy = n_distinct(CELLCODE))%>%
  select(-c(CELLCODE))%>%
  distinct()

baseline_per_RBU_EEA$state <- 'baseline'

gc()

#intersect of baseline occurences with RBSU####

baseline_in_RBSU <- raster::intersect(baseline, RBSU)
baseline_in_RBSU_EEA <- raster::intersect(baseline_in_RBSU, EEA_1km)

baseline_in_RBSU_EEA_data <- as.data.frame(baseline_in_RBSU_EEA@data)

baseline_per_RBSU_EEA <- baseline_in_RBSU_EEA_data %>%
  select(scientific_name, A0_CODE, CELLCODE)%>% 
  group_by (scientific_name, A0_CODE)%>%
  mutate(Occupancy = n_distinct(CELLCODE))%>%
  select(-c(CELLCODE))%>%
  distinct()

baseline_per_RBSU_EEA$state <- 'baseline'

gc()

#intersect of current state occurences with RBU####
current_in_RBU <- raster::intersect(current_state, RBU)
current_in_RBU_EEA <- raster::intersect(current_in_RBU, EEA_1km)

current_in_RBU_EEA_data <- as.data.frame(current_in_RBU_EEA@data)

names(current_in_RBU_EEA_data)[10] <-'RBU'

current_per_RBU_EEA <- current_in_RBU_EEA_data %>%
  select(scientific_name, RBU, CELLCODE)%>% 
  group_by (scientific_name, RBU)%>%
  mutate(Occupancy = n_distinct(CELLCODE))%>%
  select(-c(CELLCODE))%>%
  distinct()

current_per_RBU_EEA$state <- 'current'

gc()
#intersect of current state occurences with RBSU####
current_in_RBSU <- raster::intersect(current_state, RBSU)
current_in_RBSU_EEA <- raster::intersect(current_in_RBSU, EEA_1km)

current_in_RBSU_EEA_data <- as.data.frame(current_in_RBSU_EEA@data)

current_per_RBSU_EEA <- current_in_RBSU_EEA_data %>%
  select(scientific_name,A0_CODE, CELLCODE)%>% 
  group_by (scientific_name, A0_CODE)%>%
  mutate(Occupancy = n_distinct(CELLCODE))%>%
  select(-c(CELLCODE))%>%
  distinct()

current_per_RBSU_EEA$state <- 'current'

gc()
#bind tables####
occupancy_RBU <- rbind(current_per_RBU_EEA, baseline_per_RBU_EEA)

occupancy_RBSU <- rbind(current_per_RBSU_EEA, baseline_per_RBSU_EEA)

#join occupancy files with observations files
occupancy_RBSU<- merge(occupancy_RBSU, observations_RBSU, by= c('scientific_name', 'A0_CODE', 'state'),all.x=TRUE)
occupancy_RBU <- merge(occupancy_RBU, observations_RBU, by= c('scientific_name', 'A0_CODE', 'state'),all.x=TRUE)

#save output####

write.csv(occupancy_RBU, './data/interim/occupancy_RBU.csv', row.names=FALSE)
write.csv(occupancy_RBSU, './data/interim/occupancy_RBSU.csv', row.names=FALSE)

#test_barplot####

occupancy_RBU_DIJLE <- occupancy_RBU%>%
  filter(RBU== 'Dijle - Dyle')

library(ggplot2)
p<-ggplot(data= occupancy_RBU_DIJLE, aes(x=scientific_name, y=Occupancy, fill= state)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal() +
  scale_fill_brewer(palette="Paired")+
  coord_flip()

show(p)
