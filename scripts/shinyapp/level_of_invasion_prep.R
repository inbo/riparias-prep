library(leaflet)
library(rgdal)
library(sf)
library(dplyr)

branch <- "41_extending_baseline_map"

#import relative occupancy
occupancy_RBU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBU.csv"))
occupancy_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBSU.csv"))

#remove first column
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

#export output
write.csv(occupancy_RBU_temp, './data/interim/occupancy_RBU.csv')
write.csv(occupancy_RBSU_temp, './data/interim/occupancy_RBSU.csv')


