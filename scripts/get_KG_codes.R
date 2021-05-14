get_KG_codes <- function(region = "belgium"){
  # Setup ####
  library(sp)
  library(rgdal)
  library(raster)
  library(rworldmap)
  library(leaflet)
  library(bcmaps)
  library(tidyverse)
  
  crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Prep region ####
  region <- tolower(region)
  
  # Get Belgian Border ####
  worldmap <- getMap(resolution = "low")
  region_border <- subset(worldmap, tolower(worldmap$ADMIN) == region) 
  region_border <- spTransform(region_border, crs_wgs)
  
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
    
    girdcode_intersect <- raster::intersect(shape, belgian_border)
    
    for(g in girdcode_intersect@data$GRIDCODE){
      output <- output %>% 
        add_row(scenario = s,
                KG_GridCode = g)
    }
  }
  
  output <- output %>% 
    left_join(legend, by = c("KG_GridCode" = "GRIDCODE"))
  
  # Export output ####
  write_csv(output, paste0("./data/interim/", region, "_KG_codes.csv"))
  return(output)
}