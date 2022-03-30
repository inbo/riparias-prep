library(leaflet)
library(rgdal)
library(sf)
library(dplyr)

branch <- "41_extending_baseline_map"

#import relative occupancy
occupancy_RBU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBU.csv"))
occupancy_RBSU <- read.csv(paste0("https://github.com/inbo/riparias-prep/raw/", branch, "/data/interim/occupancy_RBSU.csv"))

occupancy_RBSU <- occupancy_RBSU %>% select(-c(0,1))


#save output
write.csv(level_of_invasion_RBU, './data/interim/occupancy_RBU.csv')
write.csv(level_of_invasion_RBSU, './data/interim/occupancy_RBSU.csv')


