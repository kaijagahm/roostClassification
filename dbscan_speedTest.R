# Speed testing for dbscan clustering
# Created 2023-01-03 to test computational time, in preparation for clustering all roosts for Marta.
# Starting out, I have all data from 2020-08-28 through 2022-12-31

library(tidyverse)
library(fpc) # for DBSCAN clustering



# Load data
load("data/simplifiedRoosts.Rda")

# 1000m, 3pts, all data
# 1000m, 3pts, 10% data
# 1000m, 3pts, 25% data
# 1000m, 3pts, 50% data
# 1000m, 3pts, 75% data
# 1000m, 3pts, 90% data

# eps is the reachability distance. I'm going to consider that to be 500m for starters. minPts for a core point is going to be 3.
dbscan <- dbscan(distanceMatrix, eps = 750, method = "dist", MinPts = 3)

data_dbscan <- bind_cols(testDF, "cluster" = dbscan$cluster) %>%
  mutate(isOutlier = case_when(cluster == 0 ~ T,
                               TRUE ~ F))

# plot
data_dbscan %>%
  ggplot(aes(x = location_long, y = location_lat, col = factor(cluster)))+
  geom_point(alpha = 0.3, aes(shape = isOutlier))+
  scale_shape_manual(values = c(19, 3))+
  theme_classic()+
  theme(legend.position = "none")+
  coord_equal()

# zoom in
data_dbscan %>%
  filter(location_lat < 31.8, location_lat > 30.5, location_long < 35.4, location_long > 34.6) %>%
  ggplot(aes(x = location_long, y = location_lat, col = factor(cluster)))+
  geom_point(alpha = 0.3, aes(shape = isOutlier))+
  scale_shape_manual(values = c(19, 3))+
  theme_classic()+
  theme(legend.position = "none")+
  coord_equal()


How many points are classified as outliers?
  
  
nPoints <- nrow(testDF)
dists <- c(100, 250, 500, 750, 1000, 1500)
minpts <- 3
dbscans_list <- vector(mode = "list", length = length(dists))
for(i in 1:length(dists)){
  dbscans_list[[i]] <- fpc::dbscan(distanceMatrix, eps = dists[i], method = "dist", MinPts = minpts)
}
data_dbscans <- map(dbscans_list, ~bind_cols(testDF, "cluster" = .x$cluster) %>%
                      mutate(isOutlier = ifelse(cluster == 0, T, F)))
nOutliers <- map_dbl(data_dbscans, ~.x %>%
                       filter(cluster == 0) %>%
                       nrow())
outliers <- data.frame(dist = dists, 
                       nOutliers = nOutliers,
                       propOutliers = nOutliers/nPoints)

outliers %>%
  ggplot(aes(x = dist, y = propOutliers))+
  geom_point(size = 2)+
  geom_line()+
  theme_classic()+
  ylab("Proportion of points not included in a cluster")+
  xlab("Neighborhood distance (m)")

Make zoomed-in plots for each dbscan distance

set.seed(1)
zoomPlots <- map2(data_dbscans, dists, ~{
  yr <- min(lubridate::year(.x$roost_date))
  .x %>%
    filter(location_lat < 31.8, location_lat > 30.5, location_long < 35.4, location_long > 34.6) %>%
    ggplot(aes(x = location_long, y = location_lat, col = factor(cluster)))+
    geom_point(alpha = 0.3, aes(shape = isOutlier))+
    scale_shape_manual(values = c(19, 3))+
    theme_classic()+
    theme(legend.position = "none")+
    coord_equal()+
    ggtitle(paste0("Year = ", yr, "; MinPts = ", minpts, "; eps = ", .y, "m"))+
    scale_color_paletteer_d("trekcolors::lcars_cardassian")
})

walk(zoomPlots, print)