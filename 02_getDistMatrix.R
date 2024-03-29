# Script to get distance matrix from roost locations. This will take a really long time.

library(tidyverse)
library(sf)
library(beepr)

# Load data
load("data/simplifiedRoosts.Rda")
martaRoosts <- read.csv("data/Roosts_df_mod.csv")
martaRoosts_oldies <- read.csv("data/Roosts_df_mod_oldies.csv")
allRoosts <- read.csv("data/Roosts_all.csv")

# Create a geographical distance matrix (this is the step that takes a really long time)
sf <- simplifiedRoosts %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84")

sfMarta <- martaRoosts %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84")

sfMartaOldies <- martaRoosts_oldies %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84")

sfAll <- allRoosts %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F) %>%
  sf::st_set_crs("WGS84")

# Compute pairwise distances (my roosts)
# distanceMatrix <- sf::st_distance(sf, sf) %>% # this takes a really really long time with such a big dataset.
#   as.dist()
# distanceMatrix <- as.matrix(distanceMatrix)
# save(distanceMatrix, file = "data/distanceMatrix.Rda")

# Compute pairwise distances (Marta's roosts)
distanceMatrixMarta <- sf::st_distance(sfMarta, sfMarta) %>%
  as.dist() %>%
  as.matrix()
Roosts_df_mod_distanceMatrix <- distanceMatrixMarta
save(Roosts_df_mod_distanceMatrix, file = "data/Roosts_df_mod_distanceMatrix.Rda")

# Compute pairwise distances (Marta's roosts, with older individuals)
distanceMatrixMartaOldies <- sf::st_distance(sfMartaOldies, sfMartaOldies) %>%
  as.dist() %>%
  as.matrix()
Roosts_df_mod_distanceMatrix_oldies <- distanceMatrixMartaOldies
save(Roosts_df_mod_distanceMatrix_oldies, file = "data/Roosts_df_mod_distanceMatrix_oldies.Rda")

