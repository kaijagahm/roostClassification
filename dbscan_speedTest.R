# Speed testing for dbscan clustering
# Created 2023-01-03 to test computational time, in preparation for clustering all roosts for Marta.
# Starting out, I have all data from 2020-08-28 through 2022-12-31

library(tidyverse)
library(fpc) # for DBSCAN clustering
library(sf)

# Load data
load("data/simplifiedRoosts.Rda")
load("data/distanceMatrix.Rda")
distanceMatrix <- as.matrix(distanceMatrix)
N <- nrow(distanceMatrix)

# Make a list to hold the data and its percentages
pcts <- c(1, .1, .25, .5, .75)
whiches <- map(pcts, ~sample(1:N, size = round(N*.x), replace = F))
pctList <- map(whiches, ~distanceMatrix[.x, .x])

# Run dbscan as a loop so I can evaluate the runtime
starts <- rep(NA, length(pcts))
ends <- rep(NA, length(pcts))
dbscans <- vector(mode = "list", length = length(pcts))
pts <- 3
eps <- 1000
for(i in 1:length(pctList)){
  start <- Sys.time()
  out <- dbscan(pctList[[i]], eps = eps, method = "dist", MinPts = pts)
  end <- Sys.time()
  starts[[i]] <- start
  ends[[i]] <- end
  dbscans[[i]] <- out
  cat(paste("Round", i, "done!"))
}
durations <- (ends-starts)/60

# Make a plot to see how long that took. Note to self: it actually didn't take nearly as long as I thought it would! Seems like computing the distance matrix was really the most difficult/time consuming step.
dat <- data.frame(pct = pcts,
                  nPts = map_dbl(whiches, length),
                  duration = durations) %>%
  mutate(nDists = nPts^2)

dat %>%
  ggplot(aes(x = nPts, y = duration))+
  geom_point()+
  geom_line()+
  theme_classic()

dat %>%
  ggplot(aes(x = nDists, y = duration))+
  geom_point()+
  geom_line()+
  theme_classic()
# Hooray, I have discovered that these computations scale exponentially. DUH. The good news is, the runtime isn't actually that long (a couple of minutes! Very tractable!) even for quite a lot of data.
# The rate-limiting step is undoubtedly going to be the distance matrix computation.