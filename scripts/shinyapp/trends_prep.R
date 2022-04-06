
# Setup ####
## Libraries ####
library(tidyverse) # To do data science
library(tidylog) # To provide feedback on dplyr functions
library(progress) # To add progress bars
library(here) # To find files
library(lubridate) # To work with dates
library(rgdal)
library(rgbif)
library(trias)

## CRS ####
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Read data ####
points_in_perimeter <- readOGR("./data/spatial/baseline/points_in_perimeter.geojson", stringsAsFactors = FALSE)

EEA_1km <- readOGR("data/spatial/EEA 1km/be_1km.shp")
EEA_1km <- spTransform(EEA_1km, crs_wgs)

# trias prep ####
## column namens ####
points_in_perimeter@data <- points_in_perimeter@data %>% 
  rename(taxonKey = accptTK)

## grid cells ####
sp_grid <- raster::intersect(points_in_perimeter, EEA_1km)

df_grid <- sp_grid@data %>% 
  rename(eea_cell_code = CELLCODE) %>% 
  group_by(taxonKey, eea_cell_code, year) %>% 
  summarise(obs = n()) 

## classinfo ####
taxon_key <-
  points_in_perimeter@data %>%
  distinct(taxonKey) %>% 
  pull()

spec_names <- data.frame()
for(k in taxon_key){
  spec_names_temp <- name_usage(key = k)
  spec_names_temp <- spec_names_temp$data %>% 
    select(taxonKey = key,
           canonicalName,
           scientificName,
           kingdomKey, 
           kingdom,
           classKey,
           class)%>%
    mutate(canonicalName = ifelse(
      is.na(canonicalName), scientificName, canonicalName
    ))
  if(nrow(spec_names)==0){
    spec_names <- spec_names_temp
  }else{
    spec_names <- rbind(spec_names, spec_names_temp)
  }
}

## baseline ####
df_bl <- read_csv(
  file = "https://raw.githubusercontent.com/trias-project/occ-cube-alien/master/data/processed/be_classes_cube.csv",
  col_types = cols(
    year = col_double(),
    eea_cell_code = col_character(),
    classKey = col_double(),
    n = col_double(),
    min_coord_uncertainty = col_double()
  ),
  na = ""
)
df_bl <-
  df_bl %>%
  rename(cobs = n)

## timeseries ####
df_cc <- 
  df_grid %>%
  group_by(taxonKey) %>%
  distinct(eea_cell_code) %>%
  ungroup()
df_begin_year <- 
  df_grid %>%
  group_by(taxonKey) %>%
  summarize(begin_year = min(year))
df_cc <- 
  df_cc %>%
  left_join(df_begin_year, by = "taxonKey") %>%
  select(taxonKey, begin_year, eea_cell_code)
make_time_series <- function(eea_cell_code, taxonKey, begin_year, last_year ) {
  expand_grid(eea_cell_code = eea_cell_code,
              taxonKey = taxonKey,
              year = seq(from = begin_year, to = last_year))
  
}

df_ts <- pmap_dfr(df_cc, 
                  .f = make_time_series, 
                  last_year = year(Sys.Date())
)

df_ts <- 
  df_ts %>%
  left_join(df_grid %>% select(taxonKey, year, eea_cell_code, obs), 
            by = c("taxonKey", "year", "eea_cell_code"))

df_ts <- 
  df_ts %>%
  left_join(spec_names %>% 
              select(taxonKey, classKey), 
            by = "taxonKey")
## observer bias ####
df_ts <- 
  df_ts %>%
  left_join(df_bl %>%
              select(year, eea_cell_code, classKey, cobs),
            by = c("year", "eea_cell_code", "classKey")) %>%
  mutate(cobs = cobs - obs)
df_ts <-
  df_ts %>%
  replace_na(list(cobs = 0, obs = 0))
df_ts <- 
  df_ts %>%
  mutate(pa_cobs = if_else(cobs > 0, 1, 0),
         pa_obs = if_else(obs > 0, 1, 0))
df_ts <-
  df_ts %>%
  select(taxonKey, 
         year, 
         eea_cell_code, 
         obs, 
         pa_obs, 
         cobs, 
         pa_cobs,
         classKey)

## modelling prep ####
df_ts_compact <-
  df_ts %>%
  group_by(taxonKey, year, classKey) %>%
  summarise(
    obs = sum(obs),
    cobs = sum(cobs),
    ncells = sum(pa_obs),
    c_ncells = sum(pa_cobs)
  ) %>%
  ungroup()

df_ts_compact <-
  df_ts_compact %>%
  left_join(spec_names, by = "taxonKey")

# export ####
write_csv(df_ts_compact, "./data/interim/trends_compact.csv")
