library(leaflet)
library(rgdal)
library(sf)
library(dplyr)
library(tidyr)

branch <- "41_extending_baseline_map"

#import geojson of RBU and RBSU####
RBU <- st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/",
                               branch,
                               "/data/spatial/perimeter/Riparias_Official_StudyArea.geojson"), stringsAsFactors = FALSE))
RBSU <- st_as_sf(readOGR(paste0("https://github.com/inbo/riparias-prep/raw/",
                                branch,
                                "/data/spatial/Riparias_subunits/Final_RSU_RIPARIAS_baseline.geojson"), stringsAsFactors = FALSE))

#import relative occupancy####
occupancy_RBU <- read.csv(paste0("./data/interim/occupancy_RBU.csv"))
occupancy_RBSU <- read.csv(paste0("./data/interim/occupancy_RBSU.csv"))

#remove first column####
occupancy_RBU <- occupancy_RBU %>% select(-c(1))
occupancy_RBSU <- occupancy_RBSU %>% select(-c(1))
 
#define level of invasion depending on relative occupancy
occupancy_RBU_temp <- occupancy_RBU %>%
  mutate(
  level_of_invasion = case_when(
    Occupancy_rel == 0 ~ "uninvaded",
    Occupancy_rel <= 0.10 ~ "scattered occurences only",
    Occupancy_rel <= 0.30 ~ "weakly invaded",
    Occupancy_rel > 0.30 ~ "heavily invaded")
  )
   
occupancy_RBSU_temp <- occupancy_RBSU %>%
  mutate(
    level_of_invasion = case_when(
      Occupancy_rel == 0 ~ "uninvaded",
      Occupancy_rel <= 0.10 ~ "scattered occurences only",
      Occupancy_rel <= 0.30 ~ "weakly invaded",
      Occupancy_rel > 0.30 ~ "heavily invaded")
  )

#add level of invasion (as value) per species (as variable) to spatial RBSU layer
occupancy_RBSU_temp<- occupancy_RBSU %>% select(c('A0_CODE', 'scientific_name', 'state', 'level_of_invasion'))
occupancy_RBSU_wider <- occupancy_RBSU_temp %>% pivot_wider(names_from= "scientific_name", values_from= "level_of_invasion")

level_of_invasion_RBSU <- merge(RBSU, occupancy_RBSU_wider, by='A0_CODE')

occupancy_RBU_temp<- occupancy_RBU %>% select(c('RBU', 'scientific_name', 'state', 'level_of_invasion'))
occupancy_RBU_wider <- occupancy_RBU_temp %>% pivot_wider(names_from= "scientific_name", values_from= "level_of_invasion")

level_of_invasion_RBU <- merge(RBU, occupancy_RBU_wider, by.x='BEKNAAM', by.y="RBU")

#export output
st_write(level_of_invasion_RBSU,"./data/interim/level_of_invasion_RBSU.geojson", append=FALSE)
st_write(level_of_invasion_RBU, "./data/interim/level_of_invasion_RBU.geojson", append=FALSE)

write.csv(occupancy_RBU_temp, './data/interim/occupancy_RBU.csv')
write.csv(occupancy_RBSU_temp, './data/interim/occupancy_RBSU.csv')


