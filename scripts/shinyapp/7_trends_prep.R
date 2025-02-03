
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
branch <- "74_fixes_dashboard"

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
# download de nieuwe versie van de classes cube en lees deze in
# creëer de query
# query <- "SELECT \"year\", GBIF_EEARGCode( 1000, decimalLatitude, decimalLongitude, COALESCE(coordinateUncertaintyInMeters, 1000) ) AS eeaCellCode, classKey, class, COUNT(*) AS occurrences, MIN(COALESCE(coordinateUncertaintyInMeters, 1000)) AS minCoordinateUncertaintyInMeters, MIN(GBIF_TemporalUncertainty(eventDate)) AS minTemporalUncertainty FROM occurrence WHERE occurrenceStatus = 'PRESENT' AND countrycode = 'BE' AND \"year\" >= 2000 AND hasCoordinate = TRUE AND NOT ARRAY_CONTAINS(issue, 'ZERO_COORDINATE') AND NOT ARRAY_CONTAINS(issue, 'COORDINATE_OUT_OF_RANGE') AND NOT ARRAY_CONTAINS(issue, 'COORDINATE_INVALID') AND NOT ARRAY_CONTAINS(issue, 'COUNTRY_COORDINATE_MISMATCH') AND (LOWER(identificationVerificationStatus) NOT IN ( 'unverified', 'unvalidated', 'not validated', 'under validation', 'not able to validate', 'control could not be conclusive due to insufficient knowledge', 'uncertain', 'unconfirmed', 'unconfirmed - not reviewed', 'validation requested' ) OR identificationVerificationStatus IS NULL)AND classKey IN (<--nubkeys-->)AND coordinateUncertaintyInMeters <= 10000GROUP BY \"year\", eeaCellCode, classKey, class ORDER BY \"year\" DESC, eeaCellCode ASC;"
# 
# # bepaal de classes die in de baseline zitten
# classes <- spec_names$classKey %>% 
#   unique() %>% 
#   paste(collapse = ", ")
# 
# # voeg de classes toe aan de query
# query <- gsub("<--nubkeys-->", classes, query)
# 
# # download de classes cube
# df_bl <- occ_download_sql(q = query, 
#                           format = "SQL_TSV_ZIP",
#                           email = Sys.getenv("gbif_email"),
#                           user = Sys.getenv("gbif_user"),
#                           pwd = Sys.getenv("gbif_pwd"))
# 
# occ_download_wait(df_bl)
# df_bl_raw <- occ_download_get(df_bl, overwrite = TRUE) %>%
#   occ_download_import() 

# download previous update of the classes cube and read it in
df_bl_raw <- occ_download_get("0006958-250127130748423", overwrite = TRUE) %>%
  occ_download_import() 

# Select gridcells within perimeter
df_bl <- df_bl_raw %>% 
  filter(eeacellcode %in% EEA_1km$CELLCODE) %>%
  rename(cobs = occurrences,
         eea_cell_code = eeacellcode,
         classKey = classkey)

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
