#'This script loads two csv-files (occupancy_RBU.csv and occupancy_RBSU.csv) 
#'and adds extra columns two the files describing the occupancy per species per area per state
#'Two output files are generated:
#'occupancy_RBU.csv: extra columns describe total number of EEA cells per area and relative occupancy per species per river basin unit per state (baseline or current) 
#'header observations_RBU.csv: scientific_name,"RBU","state", "n_observations","Occupancy", "Total_cellcode_per_area", "Occupancy_rel"
#'occupancy_RBSU.csv: extra columns describe total number of EEA cells per area and relative occupancy  per species per river basin subunit per state (baseline or current)
#'header observations_RBSU.csv: scientific_name,"A0_CODE","state","n_observations", "Occupancy", "Total_cellcode_per_area", Occupancy_rel"
#'
#'@param branch A string referring to the branch on github on which input data is read in
#'@param RBU geojson file in WGS 84 projection containing polygons at river basin unit level
#'@param RBSU geojson file in WGS 84 projection containing polygons at river basin subunit level
#'@param EEA_1km


library(leaflet)
library(rgdal)
library(sf)
library(dplyr)

branch <- "41_extending_baseline_map"

RBU <- st_as_sf(
  readOGR(paste0("https://github.com/inbo/riparias-prep/raw/",
                 branch,
                 "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), 
          stringsAsFactors = FALSE)
)

RBSU <- st_make_valid(
  st_as_sf(
  readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", 
                 branch,
                 "/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"),
          stringsAsFactors = FALSE)
  )
)

#correct projection####
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

#import EEA 1km file####
EEA_1km <- readOGR("data/spatial/EEA 1km/be_1km.shp")
EEA_1km <- st_as_sf(spTransform(EEA_1km, crs_wgs))

#intersect of EEA_1km with RBU####
EEA_1km_in_RBU <- st_intersection(EEA_1km, RBU)

EEA_1km_RBU_data <- as.data.frame(EEA_1km_in_RBU)

#check if outcome is logical####
table(table(EEA_1km_RBU_data$CELLCODE))

names(EEA_1km_RBU_data)[4] <-'RBU'

CELLES_per_RBU <- EEA_1km_RBU_data %>%
  select(RBU, CELLCODE)%>%
  group_by (RBU)%>%
  summarise(Total_cellcode_per_area=n())

gc()

#intersect of EEA_1km with RBSU####
EEA_1km_in_RBSU <- st_intersection(EEA_1km, RBSU)

EEA_1km_RBSU_data <- as.data.frame(EEA_1km_in_RBSU)

#check if outcome is logical####
table(table(EEA_1km_RBSU_data$CELLCODE))

CELLES_per_RBSU <- EEA_1km_RBSU_data %>%
  select(A0_CODE, CELLCODE)%>%
  group_by (A0_CODE)%>%
  summarise(Total_cellcode_per_area=n())

gc()

#import absolute output####
occupancy_RBU <- read.csv("./data/interim/occupancy_RBU.csv")
occupancy_RBSU <- read.csv("./data/interim/occupancy_RBSU.csv")

#merge both tables####
occupancy_RBU_temp <- merge(occupancy_RBU, CELLES_per_RBU, by='RBU')
occupancy_RBSU_temp <- merge(occupancy_RBSU, CELLES_per_RBSU, by='A0_CODE')

#generate relative occupancy####
occupancy_RBU_temp$Occupancy_rel <- occupancy_RBU_temp$Occupancy/occupancy_RBU_temp$Total_cellcode_per_area
occupancy_RBSU_temp$Occupancy_rel <- occupancy_RBSU_temp$Occupancy/occupancy_RBSU_temp$Total_cellcode_per_area

#save output####
write.csv(occupancy_RBU_temp, './data/interim/occupancy_RBU.csv', row.names=FALSE)
write.csv(occupancy_RBSU_temp, './data/interim/occupancy_RBSU.csv', row.names=FALSE)


