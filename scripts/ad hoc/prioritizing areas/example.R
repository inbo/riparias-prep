library(sf)
library(rgdal)
library(httr)
library(tidyverse)
#Read in waterbodies####
Wb_WAL <- st_read("./data/spatial/watervlakken/Plans_eau_PICEA_pourBDREF.gdb")# returns sf-object

Wb_Vlaanderen <- readOGR("./data/spatial/watervlakken/watervlakken.shp")#returs sp-object

WFS_BRU <- "https://wfs.environnement.brussels/belb?REQUEST=GetCapabilities&SERVICE=WFS"

url <- parse_url(WFS_BRU)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # facultative
                  request = "GetCapabilities"
)
request <- build_url(url)
request

Wb_Brussel<-read_sf(request, layer="Water_surface")#returns sf-object



#transforming data####
st_crs(Wb_WAL)#check projectie
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
Wb_WAL <- st_transform(Wb_WAL, crs_wgs)


st_crs(Wb_Vlaanderen)
Wb_Vlaanderen <- spTransform(Wb_Vlaanderen, crs_wgs)


st_crs(Wb_Brussel)
Wb_Brussel <- st_transform(Wb_Brussel, crs_wgs)

#Optioneel: buffer plaatsen rond elk watervlak


#read in observations of Riparias species in Riparias area
#TO CHECK: Are these all the species of the Riparias checklist (this file was made by Sander)

branch <- "41_extending_baseline_map"

current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/current_state.geojson"), stringsAsFactors = FALSE)

baseline_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/baseline.geojson"), stringsAsFactors = FALSE)

#INTERSECT observations with watervlakken vlaanderen:

output <- raster::intersect(current_state,Wb_Vlaanderen) #error bij mij want te veel RAM nodig en PC kan het niet aan :p

output_data <- as.data.frame(output$data)

final_result <- output_data %>%
  select(scientific_name, waterbody_name, year)%>%
  group_by()%>%
  summarize() #afhankelijk van wat geanalyzeerd moet worden



