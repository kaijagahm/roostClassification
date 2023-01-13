# DBSCAN run on Marta's data
# Created 2023-01-11

# Using Marta's modeling data, distance of 1000m, minPts threshold of 3 for now.

# Load packages
library(fpc)
library(tidyverse)
library(sf)
library(mapview)
library(paletteer)

# Load the distance matrix (computed in 02_getDistMatrix.R)
roosts <- read.csv("data/Roosts_df_mod.csv")
load("data/Roosts_df_mod_distanceMatrix.Rda")

# Do a dbscan
dbscan_3 <- dbscan(Roosts_df_mod_distanceMatrix, eps = 1000, method = "dist", MinPts = 3)
dbscan_4 <- dbscan(Roosts_df_mod_distanceMatrix, eps = 1000, method = "dist", MinPts = 4)
dbscan_5 <- dbscan(Roosts_df_mod_distanceMatrix, eps = 1000, method = "dist", MinPts = 5)
clusterAssignments_3 <- dbscan_3$cluster
clusterAssignments_4 <- dbscan_4$cluster
clusterAssignments_5 <- dbscan_5$cluster
dbscanResultsMarta <- bind_cols(roosts, "cluster_3" = clusterAssignments_3, "cluster_4" = clusterAssignments_4, "cluster_5" = clusterAssignments_5)

# Save data
save(dbscanResultsMarta, file = "data/dbscanResultsMarta.Rda")
load("data/dbscanResultsMarta.Rda")

# Make an interactive map
# Create sf dataset
dbscanResultsMarta_sf <- sf::st_as_sf(dbscanResultsMarta, coords = c("location_long", "location_lat"), crs = "WGS84")

# group and summarize by cluster, ignoring cluster 0 (outliers)
convex_3 <- dbscanResultsMarta_sf %>%
  filter(cluster_3 != 0) %>%
  group_by(cluster_3) %>%
  summarize(geometry = st_combine(geometry)) %>%
  st_convex_hull()

convex_4 <- dbscanResultsMarta_sf %>%
  filter(cluster_4 != 0) %>%
  group_by(cluster_4) %>%
  summarize(geometry = st_combine(geometry)) %>%
  st_convex_hull()

convex_5 <- dbscanResultsMarta_sf %>%
  filter(cluster_5 != 0) %>%
  group_by(cluster_5) %>%
  summarize(geometry = st_combine(geometry)) %>%
  st_convex_hull()

map <- mapview(dbscanResultsMarta_sf, layer.name = "Roost location", legend = FALSE, cex = 2, color = "black")+
  mapview(convex_3, zcol = "cluster_3", layer.name = "Convex polygons 3",
          legend = FALSE)+
  mapview(convex_4, zcol = "cluster_4", layer.name = "Convex polygons 4",
          legend = FALSE)+
  mapview(convex_5, zcol = "cluster_5", layer.name = "Convex polygons 5",
          legend = FALSE)
