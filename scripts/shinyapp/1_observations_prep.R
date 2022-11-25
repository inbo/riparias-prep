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

rm(list=ls())

#loading libraries####
library(leaflet)
library(rgdal)
library(sf)
library(dplyr)
library(ggplot2)



#read in input data####
branch <- "50_add_species"

current_state<- st_read(paste0("https://github.com/inbo/riparias-prep/raw/", 
                                branch,
                                "/data/spatial/baseline/current_state.geojson"))

baseline <- st_read(paste0("https://github.com/inbo/riparias-prep/raw/", 
                           branch,
                           "/data/spatial/baseline/baseline.geojson"))

RBU <- st_read(paste0("https://github.com/inbo/riparias-prep/raw/",
                      branch,
                      "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"))

RBSU <- st_read(paste0("https://github.com/inbo/riparias-prep/raw/",
                       branch,
                       "/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"))

RBSU <- st_make_valid(RBSU)

#intersect of baseline occurences with RBU####
baseline_in_RBU <- st_intersection(baseline, RBU)

baseline_in_RBU <- baseline_in_RBU%>%rename(RBU=BEKNAAM)


baseline_per_RBU <- baseline_in_RBU %>%
  select(species, RBU)%>%
  group_by (species, RBU)%>%
  count(species)%>%
  rename(n_observations=n)

baseline_per_RBU$state <- 'baseline'

#intersect of baseline occurences with RBSU####

baseline_in_RBSU <- st_intersection(baseline, RBSU)

baseline_in_RBSU <- baseline_in_RBSU%>%rename(RBSU=A0_CODE)


baseline_per_RBSU <- baseline_in_RBSU %>%
  select(species, RBSU)%>%
  group_by (species, RBSU)%>%
  count(species)%>%
  rename(n_observations=n)

baseline_per_RBSU$state <- 'baseline'

#intersect of current state occurences with RBU####
current_in_RBU <- st_intersection(current_state, RBU)

current_in_RBU <- current_in_RBU%>%rename(RBU=BEKNAAM)

current_per_RBU <- current_in_RBU %>%
  select(species, RBU)%>%
  group_by (species, RBU)%>%
  count(species)%>%
  rename(n_observations=n)

current_per_RBU$state <- 'current'

#intersect of current state occurences with RBSU####
current_in_RBSU <- st_intersection(current_state, RBSU)

current_in_RBSU <- current_in_RBSU%>%rename(RBSU=A0_CODE)

current_per_RBSU <- current_in_RBSU %>%
  select(species, RBSU)%>%
  group_by (species, RBSU)%>%
  count(species)%>%
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

p<-ggplot(data= overview_RBU_DIJLE, aes(x=species, y=n_observations, fill= state)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme_minimal() +
  scale_fill_brewer(palette="Paired")+
  coord_flip()

#check intersections

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=RBSU, color="black")%>%
  addPolylines(data=RBU, color= "green")
  
