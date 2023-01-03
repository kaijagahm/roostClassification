# Script to get distance matrix from roost locations. This will take a really long time.

library(tidyverse)
library(sf)

# Load data
load("data/simplifiedRoosts.Rda")

# Create a geographical distance matrix (this is the step that takes a really long time)
sf <- simplifiedRoosts %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84")

# Compute pairwise distances
distanceMatrix <- sf::st_distance(sf, sf) %>% # this takes a really really long time with such a big dataset.
  as.dist()
save(distanceMatrix, file = "data/distanceMatrix.Rda")
load("data/distanceMatrix.Rda")