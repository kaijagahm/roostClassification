# Script to get roost locations
library(vultureUtils)
library(tidyverse)

# Read in the dataset
load("data/datAnnotCleaned.Rda")

# Get roost locations
roosts <- vultureUtils::get_roosts_df(df = datAnnotCleaned, id = "trackId")

# Simplify roost locations
simplifiedRoosts <- roosts %>%
  dplyr::select(trackId, location_long, location_lat, roost_date)

# double-check that we just have one roost per individual per night
simplifiedRoosts %>%
  group_by(trackId, roost_date) %>%
  summarize(n = n()) %>%
  pull(n) %>%
  table()

# Save roost locations
save(simplifiedRoosts, file = "data/simplifiedRoosts.Rda")