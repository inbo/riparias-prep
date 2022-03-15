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

#intersect of baseline occurences with RBU
baseline_in_RBU <- raster::intersect(baseline, RBU)

baseline_in_RBU_data <- as.data.frame(baseline_in_RBU@data)

names(baseline_in_RBU_data)[10] <-'RBU'

baseline_per_RBU <- baseline_in_RBU_data %>%
  select(scientific_name, RBU)%>%
  group_by (scientific_name, RBU)%>%
  count(scientific_name)

baseline_per_RBU$state <- 'baseline'

#intersect of baseline occurences with RBSU

baseline_in_RBSU <- raster::intersect(baseline, RBSU)

baseline_in_RBSU_data <- as.data.frame(baseline_in_RBSU@data)

baseline_per_RBSU <- baseline_in_RBSU_data %>%
  select(scientific_name, A0_CODE)%>%
  group_by (scientific_name, A0_CODE)%>%
  count(scientific_name)

baseline_per_RBSU$state <- 'baseline'

#intersect of current state occurences with RBU
current_in_RBU <- raster::intersect(current_state, RBU)

current_in_RBU_data <- as.data.frame(current_in_RBU@data)

names(current_in_RBU_data)[10] <-'RBU'

current_per_RBU <- current_in_RBU_data %>%
  select(scientific_name, RBU)%>%
  group_by (scientific_name, RBU)%>%
  count(scientific_name)

current_per_RBU$state <- 'current state'

#intersect of current state occurences with RBSU
current_in_RBSU <- raster::intersect(current_state, RBSU)

current_in_RBSU_data <- as.data.frame(current_in_RBSU@data)

current_per_RBSU <- current_in_RBSU_data %>%
  select(scientific_name, A0_CODE)%>%
  group_by (scientific_name, A0_CODE)%>%
  count(scientific_name)

current_per_RBSU$state <- 'current state'

#bind tables
overview_RBU <- rbind(current_per_RBU, baseline_per_RBU)

overview_RBSU <- rbind(current_per_RBSU, baseline_per_RBSU)

#save output

write.csv(overview_RBU, './data/interim/observations_RBU.csv')
write.csv(overview_RBSU, './data/interim/observations_RBSU.csv')

#test_barplot

overview_RBU_DIJLE <- overview_RBU%>%
  filter(RBU== 'Dijle - Dyle')

p<-ggplot(data= overview_RBU_DIJLE, aes(x=scientific_name, y=n, fill= state)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal() +
  scale_fill_brewer(palette="Paired")+
  coord_flip()

