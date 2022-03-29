library(leaflet)
library(rgdal)
library(sf)
library(dplyr)

branch <- "41_extending_baseline_map"

#current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/current_state.geojson"), stringsAsFactors = FALSE)

#baseline <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/baseline.geojson"), stringsAsFactors = FALSE)

RBU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE)

RBSU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch,"/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"), stringsAsFactors = FALSE)

#import EEA 1km file
EEA_1km <- readOGR("data/spatial/EEA 1km/be_1km.shp")
EEA_1km <- spTransform(EEA_1km, crs_wgs)

#intersect of EEA_1km with RBU
EEA_1km_in_RBU <- raster::intersect(EEA_1km, RBU)

EEA_1km_RBU_data <- as.data.frame(EEA_1km_in_RBU@data)

names(EEA_1km_RBU_data)[4] <-'RBU'

CELLES_per_RBU <- EEA_1km_RBU_data %>%
  select(RBU, CELLCODE)%>%
  group_by (RBU)%>%
  summarise(count=n())

gc()

#intersect of EEA_1km with RBSU
EEA_1km_in_RBSU <- raster::intersect(EEA_1km, RBSU)

EEA_1km_RBSU_data <- as.data.frame(EEA_1km_in_RBSU@data)


CELLES_per_RBSU <- EEA_1km_RBSU_data %>%
  select(A0_CODE, CELLCODE)%>%
  group_by (A0_CODE)%>%
  summarise(count=n())

gc()

#import absolute output
occupancy_RBU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBU.csv"))
occupancy_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBSU.csv"))

#merge both tables

occupancy_RBU <- merge(occupancy_RBU, CELLES_per_RBU, by='RBU')
occupancy_RBSU <- merge(occupancy_RBSU, CELLES_per_RBSU, by='A0_CODE')

#generate relative occupancy
occupancy_RBU$Occupancy_abs <- occupancy_RBU$Occupancy/occupancy_RBU$count
occupancy_RBSU$Occupancy_abs <- occupancy_RBSU$Occupancy/occupancy_RBSU$count

#save output
write.csv(occupancy_RBU, './data/interim/occupancy_RBU.csv')
write.csv(occupancy_RBSU, './data/interim/occupancy_RBSU.csv')


