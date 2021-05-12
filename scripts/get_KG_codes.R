# Setup ####
library(sp)
library(rgdal)
library(raster)
library(rworldmap)
library(leaflet)
library(bcmaps)
library(tidyverse)

crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Get Belgian Border ####
worldmap <- getMap(resolution = "low")
belgian_border <- subset(worldmap, tolower(worldmap$ADMIN) == "belgium") 
belgian_border <- spTransform(belgian_border, crs_wgs)

#leaflet(belgian_border) %>% 
 # addTiles() %>% 
  #addPolygons()

# Get Legend ####
legend <- read_delim("./data/spatial/koppen-geiger/KG_Rubel-Kotteks_Legend.csv", ";")
  
# Create scenario - list ####
scenario_list <- dir(path = "./data/spatial/koppen-geiger/future/",
                     pattern = ".dbf",
                     full.names = FALSE)

scenario_list <- gsub(pattern = ".dbf", 
                      replacement = "",
                      scenario_list)

# Create empty output ####

output <- data.frame() %>% 
  mutate(scenario = "",
         KG_GridCode = as.integer(""))

# Calculate KG codes ####
for(s in scenario_list){
  shape <- readOGR(dsn = "./data/spatial/koppen-geiger/future", 
                   layer = s,
                   stringsAsFactors = FALSE)
  shape <- spTransform(shape, crs_wgs)
  shape <- fix_geo_problems(shape, tries = 5)
  shape@data <- shape@data %>% 
    mutate(GRIDCODE = as.integer(GRIDCODE)) 
  
  bel_girdcode_intersect <- raster::intersect(shape, belgian_border)
  
  for(g in bel_girdcode_intersect@data$GRIDCODE){
    output <- output %>% 
      add_row(scenario = s,
              KG_GridCode = g)
  }
}

output <- output %>% 
  left_join(legend, by = c("KG_GridCode" = "GRIDCODE"))
# Export output ####


