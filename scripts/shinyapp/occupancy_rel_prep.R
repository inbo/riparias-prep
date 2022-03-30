library(leaflet)
library(rgdal)
library(sf)
library(dplyr)

branch <- "41_extending_baseline_map"

#current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/current_state.geojson"), stringsAsFactors = FALSE)

#baseline <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/baseline.geojson"), stringsAsFactors = FALSE)

RBU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE)

RBSU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch,"/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"), stringsAsFactors = FALSE)

#correct projection
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#import EEA 1km file
EEA_1km <- readOGR("data/spatial/EEA 1km/be_1km.shp")
EEA_1km <- spTransform(EEA_1km, crs_wgs)

#intersect of EEA_1km with RBU
EEA_1km_in_RBU <- raster::intersect(EEA_1km, RBU)

EEA_1km_RBU_data <- as.data.frame(EEA_1km_in_RBU@data)


#check if outcome is logical
table(table(EEA_1km_RBU_data$CELLCODE))

names(EEA_1km_RBU_data)[4] <-'RBU'

CELLES_per_RBU <- EEA_1km_RBU_data %>%
  select(RBU, CELLCODE)%>%
  group_by (RBU)%>%
  summarise(count=n())

gc()

#intersect of EEA_1km with RBSU
EEA_1km_in_RBSU <- raster::intersect(EEA_1km, RBSU)

EEA_1km_RBSU_data <- as.data.frame(EEA_1km_in_RBSU@data)

#check if outcome is logical
table(table(EEA_1km_RBSU_data$CELLCODE))

CELLES_per_RBSU <- EEA_1km_RBSU_data %>%
  select(A0_CODE, CELLCODE)%>%
  group_by (A0_CODE)%>%
  summarise(count=n())

gc()

#import absolute output
occupancy_RBU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBU.csv"))
occupancy_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBSU.csv"))

#remove first column
occupancy_RBU <- occupancy_RBU %>% select(-c(1))
occupancy_RBSU <- occupancy_RBSU %>% select(-c(1))
#merge both tables

occupancy_RBU_temp <- merge(occupancy_RBU, CELLES_per_RBU, by='RBU')
occupancy_RBSU_temp <- merge(occupancy_RBSU, CELLES_per_RBSU, by='A0_CODE')

#generate relative occupancy
occupancy_RBU_temp$Occupancy_rel <- occupancy_RBU_temp$Occupancy/occupancy_RBU_temp$count
occupancy_RBSU_temp$Occupancy_rel <- occupancy_RBSU_temp$Occupancy/occupancy_RBSU_temp$count

#save output
write.csv(occupancy_RBU_temp, './data/interim/occupancy_RBU.csv')
write.csv(occupancy_RBSU_temp, './data/interim/occupancy_RBSU.csv')


