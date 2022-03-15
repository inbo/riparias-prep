library(leaflet)
library(rgdal)
library(sf)
library(dplyr)

branch <- "41_extending_baseline_map"

current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/current_state.geojson"), stringsAsFactors = FALSE)

baseline <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/baseline.geojson"), stringsAsFactors = FALSE)

RBU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE)

#bounding box
bbox <- as.data.frame(RBU@bbox)

RBSU <- readOGR("data/spatial/Riparias subunits/River_subunits_RSU_21012021.shp", stringsAsFactors = FALSE)

#transform RBSU to projection WGS84
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

RBSU <- spTransform(RBSU, crs_wgs)

#import EEA 1km file
EEA_1km <- readOGR("data/spatial/EEA 1km/be_1km.shp")
EEA_1km <- spTransform(EEA_1km, crs_wgs)

#intersect of baseline occurences with RBU
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

#intersect of baseline occurences with RBSU

baseline_in_RBSU <- raster::intersect(baseline, RBSU)
baseline_in_RBSU_EEA <- raster::intersect(baseline_in_RBSU, EEA_1km)

baseline_in_RBSU_EEA_data <- as.data.frame(baseline_in_RBSU_EEA@data)

names(baseline_in_RBSU_EEA_data)[10] <-'RBSU'

baseline_per_RBSU_EEA <- baseline_in_RBSU_EEA_data %>%
  select(scientific_name, RBSU, CELLCODE)%>% 
  group_by (scientific_name, RBSU)%>%
  mutate(Occupancy = n_distinct(CELLCODE))%>%
  select(-c(CELLCODE))%>%
  distinct()

baseline_per_RBSU_EEA$state <- 'baseline'

gc()

#intersect of current state occurences with RBU
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
#intersect of current state occurences with RBSU
current_in_RBSU <- raster::intersect(current_state, RBSU)
current_in_RBSU_EEA <- raster::intersect(current_in_RBSU, EEA_1km)

current_in_RBSU_EEA_data <- as.data.frame(current_in_RBSU_EEA@data)

names(current_in_RBSU_EEA_data)[10] <-'RBSU'

current_per_RBSU_EEA <- current_in_RBSU_EEA_data %>%
  select(scientific_name, RBSU, CELLCODE)%>% 
  group_by (scientific_name, RBSU)%>%
  mutate(Occupancy = n_distinct(CELLCODE))%>%
  select(-c(CELLCODE))%>%
  distinct()

current_per_RBSU_EEA$state <- 'current'

gc()
#bind tables
occupancy_RBU <- rbind(current_per_RBU_EEA, baseline_per_RBU_EEA)

occupancy_RBSU <- rbind(current_per_RBSU_EEA, baseline_per_RBSU_EEA)

#save output

write.csv(occupancy_RBU, './data/interim/occupancy_RBU.csv')
write.csv(occupancy_RBSU, './data/interim/occupancy_RBSU.csv')

#test_barplot

occupancy_RBU_DIJLE <- occupancy_RBU%>%
  filter(RBU== 'Dijle - Dyle')

library(ggplot2)
p<-ggplot(data= occupancy_RBU_DIJLE, aes(x=scientific_name, y=Occupancy, fill= state)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal() +
  scale_fill_brewer(palette="Paired")+
  coord_flip()

show(p)
