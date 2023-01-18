# DBSCAN run on Marta's data
# Created 2023-01-11

# Using Marta's modeling data, distance of 1000m, minPts threshold of 3 for now.

# Load packages
library(fpc)
library(tidyverse)
library(sf)
library(mapview)
library(paletteer)
library(concaveman) # for concave hulls

# Load the distance matrix (computed in 02_getDistMatrix.R)
roosts <- read.csv("data/Roosts_df_mod.csv")
load("data/Roosts_df_mod_distanceMatrix.Rda")

# Do dbscans from 5 to 55, by 10
minPts <- seq(5, 55, by = 10)
dbscans <- map(minPts, ~dbscan(Roosts_df_mod_distanceMatrix, eps = 1000, method = "dist", MinPts = .x))
clusterAssignments <- map_dfc(dbscans, ~.x$cluster)
nms <- paste0("cluster_", minPts)
names(clusterAssignments) <- nms
dbscanResultsMarta <- bind_cols(roosts, clusterAssignments)

# Save data
save(dbscanResultsMarta, file = "data/dbscanResultsMarta.Rda")
load("data/dbscanResultsMarta.Rda")

# Make an interactive map
# Create sf dataset
dbscanResultsMarta_sf <- sf::st_as_sf(dbscanResultsMarta, coords = c("location_long", "location_lat"), crs = "WGS84")

# Create concave hulls for each set of cluster results, using a concavity of 2.5 (a somewhat arbitrary choice).
concaveHulls <- map(nms, ~dbscanResultsMarta_sf %>%
                      filter(.data[[.x]] != 0) %>%
                      group_by(.data[[.x]]) %>%
                      group_split(.keep = T) %>%
                      map_dfr(., ~concaveman::concaveman(.x, concavity = 3)))

testMap <- mapview(dbscanResultsMarta_sf, layer.name = "Roost location", 
                   legend = FALSE, cex = 2, color = "black")+
  mapview(concaveHulls[[1]], layer.name = "Concave hulls (5pts)", color = "firebrick2", col.regions = "firebrick2",
          legend = FALSE)+
  mapview(concaveHulls[[2]], layer.name = "Concave hulls (15pts)", color = "orange", col.regions = "orange",
          legend = FALSE)+
  mapview(concaveHulls[[3]], layer.name = "Concave hulls (25pts)", color = "yellow", col.regions = "yellow",
          legend = FALSE)+
  mapview(concaveHulls[[4]], layer.name = "Concave hulls (35pts)", color = "lightgreen", col.regions = "lightgreen",
          legend = FALSE)+
  mapview(concaveHulls[[5]], layer.name = "Concave hulls (45pts)", color = "skyblue", col.regions = "skyblue",
          legend = FALSE)+
  mapview(concaveHulls[[6]], layer.name = "Concave hulls (55pts)", color = "purple", col.regions = "purple",
          legend = FALSE)

testMap # Looks great. I fundamentally don't understand why the polygons created with the lower minPts thresholds are larger than the ones created by the higher minPts thresholds. That's what Marta predicted, but it's entirely counterintuitive to me.

mapshot(testMap, url = "testMap.html")

# How many clusters do we get, for each of these?
long <- dbscanResultsMarta %>%
  pivot_longer(cols = contains("cluster"), names_to = "minPts", values_to = "cluster") %>%
  mutate(minPts = as.numeric(stringr::str_remove(minPts, "cluster_")))

summ <- long %>%
  filter(cluster != 0) %>%
  group_by(minPts, cluster) %>%
  summarize(nPoints = n())

nOutliers <- long %>%
  filter(cluster == 0) %>%
  group_by(minPts) %>%
  summarize(nOutliers = n())

nClusters <- summ %>%
  filter(cluster != 0) %>%
  group_by(minPts) %>%
  summarize(nClusters = n())

nClustersPlot <- nClusters %>%
  ggplot(aes(x = minPts, y = nClusters))+
  geom_point()+
  geom_line()+
  theme_classic()+
  ylab("# clusters")+
  xlab("Min pts per cluster") # okay, yeah, this is totally intuitive.
ggsave("nClusters_minPts.png", nClustersPlot)

nOutliersPlot <- nOutliers %>%
  ggplot(aes(x = minPts, y = nOutliers))+
  geom_point()+
  geom_line()+
  theme_classic()+
  ylab("# outliers")+
  xlab("Min pts per cluster") # okay, yeah, this is totally intuitive.
ggsave("nOutliers_minPts.png", nOutliersPlot)

ptsPerCluster <- summ %>%
  ggplot(aes(x = minPts, y = log(nPoints)))+
  geom_hline(yintercept = log(5), col = "firebrick2")+
  geom_hline(yintercept = log(15), col = "orange")+
  geom_hline(yintercept = log(25), col = "yellow")+
  geom_hline(yintercept = log(35), col = "lightgreen")+
  geom_hline(yintercept = log(45), col = "skyblue")+
  geom_hline(yintercept = log(55), col = "purple")+
  geom_boxplot(aes(group = minPts, fill = as.factor(minPts)))+
  scale_fill_manual(values = c("firebrick2", "orange", "yellow", "lightgreen", "skyblue", "purple"))+
  theme_classic()+
  theme(legend.position = "none")+
  ylab("Cluster size (log-transformed)")+
  xlab("Min pts per cluster")

ggsave("ptsPerCluster_minPts.png", ptsPerCluster)

# BOOTSTRAP
# Now let's test out a bootstrap, using 75% of the data to dbscan. 
# 10 samples, 75% of data each
# l <- map(1:10, ~sample(nrow(Roosts_df_mod_distanceMatrix), size = 0.75*nrow(Roosts_df_mod_distanceMatrix)))
# folds <- map(l, ~Roosts_df_mod_distanceMatrix[.x, .x]) # there must be a faster way to subset a matrix...
# XXX This fills up memory and doesn't work. If we're going to bootstrap, we need a better way.
