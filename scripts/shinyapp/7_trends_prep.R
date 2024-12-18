
# Setup ####
## Libraries ####
library(tidyverse) # To do data science
#library(tidylog) # To provide feedback on dplyr functions
library(progress) # To add progress bars
library(here) # To find files
library(lubridate) # To work with dates
library(sf)
library(rgdal)
library(rgbif)
library(trias)

## CRS ####
crs_wgs <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# Read data ####
#read in input data####
branch <- "50_add_species"

current_state <- st_read(paste0("https://github.com/inbo/riparias-prep/raw/", 
                               branch,
                               "/data/spatial/baseline/current_state.geojson"))

baseline <- st_read(paste0("https://github.com/inbo/riparias-prep/raw/", 
                           branch,
                           "/data/spatial/baseline/baseline.geojson"))

points_in_perimeter <- rbind(current_state, baseline)

EEA_1km <- st_read("data/spatial/Riparias_subunits/EEA_1km_Riparias.geojson") %>% 
  st_transform(crs_wgs) %>% 
  dplyr::select(CELLCODE, geometry)

# trias prep ####
## grid cells ####

points_1km <- points_in_perimeter
points_1km$eea_cell_code <- apply(sf::st_intersects(EEA_1km, 
                                               points_in_perimeter, 
                                               sparse = FALSE), 2, 
                             function(col) {EEA_1km[which(col),
                             ]$CELLCODE})

df_grid <- as.data.frame(points_1km) %>% 
  dplyr::select(-geometry) %>% 
  group_by(speciesKey, eea_cell_code, year) %>% 
  summarise(obs = n()) 

## classinfo ####
taxon_key <-
  points_in_perimeter %>%
  distinct(speciesKey) %>% 
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
  group_by(speciesKey) %>%
  distinct(eea_cell_code) %>%
  ungroup()
df_begin_year <- 
  df_grid %>%
  group_by(speciesKey) %>%
  summarize(begin_year = min(year))

df_cc <- 
  df_cc %>%
  left_join(df_begin_year, by = "speciesKey") %>%
  select(speciesKey, begin_year, eea_cell_code)
make_time_series <- function(eea_cell_code, speciesKey, begin_year, last_year ) {
  expand_grid(eea_cell_code = eea_cell_code,
              speciesKey = speciesKey,
              year = seq(from = begin_year, to = last_year))
  
}

df_ts <- pmap_dfr(df_cc, 
                  .f = make_time_series, 
                  last_year = year(Sys.Date())
)

df_ts <- 
  df_ts %>%
  left_join(df_grid %>% select(speciesKey, year, eea_cell_code, obs), 
            by = c("speciesKey", "year", "eea_cell_code"))

df_ts <- 
  df_ts %>%
  left_join(spec_names %>% 
              select(taxonKey, classKey), 
            by = c("speciesKey" = "taxonKey"))
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
  select(speciesKey, 
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
  group_by(speciesKey, year, classKey) %>%
  summarise(
    obs = sum(obs),
    cobs = sum(cobs),
    ncells = sum(pa_obs),
    c_ncells = sum(pa_cobs)
  ) %>%
  ungroup()

df_ts_compact <-
  df_ts_compact %>%
  left_join(spec_names, by = c("speciesKey" = "taxonKey")) %>% 
  rename(taxonKey = speciesKey)

# export ####
write_csv(df_ts_compact, "./data/interim/trends_compact.csv")
