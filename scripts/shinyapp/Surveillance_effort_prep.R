rm(list=ls())

#load libraries####
library(leaflet)
library(rgdal)
library(sf)
library(dplyr)

#read in data####
be_species_cube <- read_csv("data/input/be_species_cube.csv")
be_species_info <- read_csv("data/input/be_species_info.csv")

#Riparias area####
RBU_laag <- st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", 
                                    branch,
                                    "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE))
RBSU_laag <- st_make_valid(
  st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/",
                                     branch,
                                     "/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"),
                   stringsAsFactors = FALSE)))

##import EEA 1km file####
EEA_1km <- readOGR("data/spatial/EEA 1km/be_1km.shp")

##define correct projection####
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
EEA_1km <- spTransform(EEA_1km, crs_wgs)
EEA_1km <- st_as_sf(EEA_1km)

EEA_Riparias <- st_intersection(EEA_1km, RBU_laag)

#select those eea cells with more than 100 occurences of plants within Riparias area
#no filter based on year?
be_species_cube_Riparias <- be_species_cube %>%
  filter(eea_cell_code %in% EEA_Riparias$CELLCODE)%>%
  inner_join(be_species_info, by= 'speciesKey')%>%
  filter(kingdom == "Plantae")%>%
  select(eea_cell_code, speciesKey)%>%
  distinct%>%
  group_by(eea_cell_code)%>%
  summarise(totalSpeciesReported=n())%>%
  filter(totalSpeciesReported>100)%>%
  rename(CELLCODE = eea_cell_code)

#create layer with only these cells####
EEA_high_search_effort <- merge(EEA_1km, be_species_cube_Riparias, by= "CELLCODE")
st_write(EEA_high_search_effort, "data/interim/EEA_high_search_effort.geojson")

#calculate number of cells with high surveillance effort per RBSU
EEA_searched_per_RBSU <- st_intersection(EEA_high_search_effort, RBSU_laag)

Surveillance_per_RBSU <- as.data.frame(EEA_searched_per_RBSU)%>%
  select(CELLCODE, Id)%>%
  group_by(Id)%>%
  summarise(n_CellsHighEffort=n())

#calculate number of total cells per RBSU
EEA_per_RBSU <- st_intersection(EEA_1km, RBSU_laag)

Total_EEA_per_RBSU <- as.data.frame(EEA_per_RBSU)%>%
  select(CELLCODE, Id)%>%
  group_by(Id)%>%
  summarise(n_TotalCells=n())

#join both columns
Surveillance_per_RBSU <- merge(Surveillance_per_RBSU, Total_EEA_per_RBSU, by= 'Id')
Surveillance_per_RBSU$SurveillanceEffortRel <- Surveillance_per_RBSU$n_CellsHighEffort/Surveillance_per_RBSU$n_TotalCells

#export final column
write.csv(Surveillance_per_RBSU,"./data/interim/Surveillance_effort.csv", row.names=FALSE)
  