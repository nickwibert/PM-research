# K-Means Clustering of Aerosol Optical Depth Data
# Author: Nick Wibert
# Last Modified: 09/25/21



# Libraries ---------------------------------------------------------------

library(ncdf4)
library(raster)
library(ecbtools) # from GitHub, function "raster.kmeans()"
library(cluster)


# K-means clustering ------------------------------------------------------

data_dir <- "/Users/nicholasw30/Downloads/PM-kmeans/data"

setwd(data_dir)

files <- list.files(pattern=".nc")

r <- stack(files)

# take only 1% of the data
r.sample <- sampleRandom(r, size=(dim(r)[1]*dim(r)[2]*0.01), xy=TRUE, na.rm=TRUE)

xy <- r.sample[,1:2]

r.sample <- as.data.frame(r.sample)

r.sample <- SpatialPointsDataFrame(coords = xy, data = r.sample,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


r.sample@data <- data.frame(r.sample@data, k = clara(scale(r.sample@data), k=5)$clustering)

plot(r.sample$x, r.sample$y, col=r.sample$k)

