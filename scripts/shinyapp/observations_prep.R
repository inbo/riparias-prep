library(leaflet)
library(rgdal)

branch <- "41_extending_baseline_map"

current_state <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/current_state.geojson"), stringsAsFactors = FALSE)

baseline <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/baseline/baseline.geojson"), stringsAsFactors = FALSE)

RBU <- readOGR(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE)

#bounding box
bbox <- as.data.frame(RBU@bbox)

RBSU <- (st_read("data/spatial/Riparias subunits/River_subunits_RSU_21012021.shp"))


