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
  RK_legend <- read_delim("./data/spatial/koppen-geiger/KG_Rubel-Kotteks_Legend.csv", 
                          ";")
  Be_legend <- read_delim("./data/spatial/koppen-geiger/KG_Beck_Legend.csv", 
                          ";")
  
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
    
    girdcode_intersect <- raster::intersect(shape, region_border)
    
    for(g in girdcode_intersect@data$GRIDCODE){
      output <- output %>% 
        add_row(scenario = s,
                KG_GridCode = g)
    }
  }
  
  output_1 <- output %>% 
    filter(grepl(pattern = "Beck", scenario)) %>% 
    left_join(Be_legend, by = c("KG_GridCode" = "GRIDCODE"))
  
  output_2 <- output %>% 
    filter(!grepl(pattern = "Beck", scenario)) %>% 
    left_join(RK_legend, by = c("KG_GridCode" = "GRIDCODE"))
  
  output_final <- rbind(output_1, output_2)
  
  # Export output ####
  write_csv(output_final, paste0("./data/interim/", region, "_KG_codes.csv"))
  return(output_final)
}