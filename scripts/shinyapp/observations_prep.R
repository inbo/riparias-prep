#'This script returns two csv fiels:
#'observations_RBU.csv : number of observations per species per river basin unit per state (baseline or current) 
#'header observations_RBU.csv: scientific_name,"RBU","n_observations","state"
#'observations_RBSU.csv : number of observations per species per river basin subunit per state (baseline or current)
#'header observations_RBSU.csv: scientific_name,"A0_CODE","n_obseervations","state"
#'
#'@param branch A string referring to the branch on github on which input data is read in
#'@param current_state geosjon file in WGS 84 projection containing observations from 2010 until now
#'@param baseline geojson file in WGS 84 projection containing observations from 2010 to 2020
#'@param RBU geojson file containing polygons at river basin unit level
#'@param RBSU geojson file containing polygons at river basin subunit level



#loading libraries####
library(leaflet)
library(rgdal)
library(sf)
library(dplyr)




#read in input data####
branch <- "41_extending_baseline_map"

current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", 
                                branch,
                                "/data/spatial/baseline/current_state.geojson"),
                         stringsAsFactors = FALSE)

baseline <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/",
                           branch,
                           "/data/spatial/baseline/baseline.geojson"),
                    stringsAsFactors = FALSE)

RBU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/",
                      branch,
                      "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE)

RBSU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch,"/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"), stringsAsFactors = FALSE)


#intersect of baseline occurences with RBU####
baseline_in_RBU <- raster::intersect(baseline, RBU)

baseline_in_RBU_data <- as.data.frame(baseline_in_RBU@data)

names(baseline_in_RBU_data)[10] <-'RBU'

baseline_per_RBU <- baseline_in_RBU_data %>%
  select(scientific_name, RBU)%>%
  group_by (scientific_name, RBU)%>%
  count(scientific_name)%>%
  rename(n_observations=n)

baseline_per_RBU$state <- 'baseline'

#intersect of baseline occurences with RBSU####

baseline_in_RBSU <- raster::intersect(baseline, RBSU)

baseline_in_RBSU_data <- as.data.frame(baseline_in_RBSU@data)

baseline_per_RBSU <- baseline_in_RBSU_data %>%
  select(scientific_name, A0_CODE)%>%
  group_by (scientific_name, A0_CODE)%>%
  count(scientific_name)%>%
  rename(n_observations=n)

baseline_per_RBSU$state <- 'baseline'

#intersect of current state occurences with RBU####
current_in_RBU <- raster::intersect(current_state, RBU)

current_in_RBU_data <- as.data.frame(current_in_RBU@data)

names(current_in_RBU_data)[10] <-'RBU'

current_per_RBU <- current_in_RBU_data %>%
  select(scientific_name, RBU)%>%
  group_by (scientific_name, RBU)%>%
  count(scientific_name)%>%
  rename(n_observations=n)

current_per_RBU$state <- 'current'

#intersect of current state occurences with RBSU####
current_in_RBSU <- raster::intersect(current_state, RBSU)

current_in_RBSU_data <- as.data.frame(current_in_RBSU@data)

current_per_RBSU <- current_in_RBSU_data %>%
  select(scientific_name, A0_CODE)%>%
  group_by (scientific_name, A0_CODE)%>%
  count(scientific_name)%>%
  rename(n_observations=n)

current_per_RBSU$state <- 'current'

#bind tables####
overview_RBU <- rbind(current_per_RBU, baseline_per_RBU)

overview_RBSU <- rbind(current_per_RBSU, baseline_per_RBSU)

#save output####

write.csv(overview_RBU, './data/interim/observations_RBU.csv', row.names=FALSE)
write.csv(overview_RBSU, './data/interim/observations_RBSU.csv', row.names=FALSE)

#test_barplot####

overview_RBU_DIJLE <- overview_RBU%>%
  filter(RBU== 'Dijle - Dyle')

p<-ggplot(data= overview_RBU_DIJLE, aes(x=scientific_name, y=n, fill= state)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal() +
  scale_fill_brewer(palette="Paired")+
  coord_flip()

