#Riparias_laag <- readOGR(dsn= paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/spatial/Riparias subunits"), layer="Final_RSU_RIPARIAS_baseline")

#st_crs(Riparias_laag)#check projectie
#crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
#Riparias_laag <- spTransform(Riparias_laag, crs_wgs)
#Riparias_laag <- st_as_sf(Riparias_laag)
#test<- sf_geojson(Riparias_laag)
#writeOGR(test, dsn="./data/spatial/Riparias subunits/Riparias_laag.GeoJSON", driver="GeoJSON")
#st_write(Riparias_laag, "test.geojson")