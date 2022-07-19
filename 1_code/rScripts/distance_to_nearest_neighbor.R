load("0_data/manual/bird/ss_xy.rData")
load("2_pipeline/store/data_main_filter.rData")

library(sf)
library(dplyr)
x<-semi_join(ss_xy, data_main_filter)

library(matrixStats)
dist_matrix   <- st_distance(x, x)           # creates a matrix of distances
diag(dist_matrix) <- NA      # replaces 0s with NA
x$distance <- rowMins(dist_matrix, na.rm = TRUE)              # get the dist of nearest element
x$nearest  <- rowMins(dist_matrix, value = T, na.rm = TRUE)   # get the index of the nearest element
hist(x$nearest)
min(x$nearest)
max(x$nearest)
mean(x$nearest)
