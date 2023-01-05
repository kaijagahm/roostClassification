# DBSCAN parameter test for MARTA
# Created 2023-01-04

# Using the dataset from 2020 through 2022, and a distance of 1000m. Testing MinPts of 2, 3, and 4. I'm not sure 2 makes much sense, but going to try anyway.

# Load packages
library(fpc)
library(tidyverse)
library(sf)
library(paletteer)

# Load the distance matrix (computed in 02_getDistMatrix.R)
load("data/distanceMatrix.Rda")
load("data/simplifiedRoosts.Rda")

# Do dbscans with each of these parameters
minPts <- 2:5
dbscans <- map(minPts, ~dbscan(distanceMatrix, eps = 1000, method = "dist", MinPts = .x))
clusterAssignments <- map_dfc(dbscans, ~.x$cluster) # get cluster assignments for each point for each dbscan run.
names(clusterAssignments) <- paste0("minPts_", 2:5)

# Add the resulting cluster data back to the original dataset
data_dbscan <- bind_cols(simplifiedRoosts, clusterAssignments) %>%
  pivot_longer(cols = contains("minPts"), names_to = "minPts", values_to = "cluster") %>%
  mutate(isOutlier = ifelse(cluster == 0, TRUE, FALSE))

# Split into a list
minPtsList <- data_dbscan %>%
  group_by(minPts) %>%
  group_split(.keep = TRUE)

plots <- map2(.x = minPtsList, .y = minPts, ~.x %>%
      filter(location_lat < 31.8, location_lat > 30.5, 
             location_long < 35.4, location_long > 34.6) %>%
      ggplot(aes(x = location_long, y = location_lat, col = factor(cluster)))+
      geom_point(alpha = 0.3, aes(shape = isOutlier), size = 0.75)+
      scale_shape_manual(values = c(19, 3))+
      theme_classic()+
      theme(legend.position = "none")+
      coord_equal()+
      ggtitle(paste0("eps = 1000m; minPts = ", .y)))

map2(.x = minPts, .y = plots, ~ggsave(paste0("fig/2020_2022_eps1000_minPts", .x, ".png"),
                                      plot = .y))
