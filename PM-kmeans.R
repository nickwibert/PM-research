# K-Means Clustering of Aerosol Optical Depth Data
# Author: Nick Wibert
# Last Modified: 09/25/21


# Libraries ---------------------------------------------------------------

library(raster)


# K-means clustering ------------------------------------------------------

data_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/Annual"

setwd(data_dir)

names <- c("BC", "NH4", "NIT", "OM", "SO4", "SOIL", "SS")
files <- c()

# get annual data for all components in the year 2000
for (name in names)
{
  files <- append(files,
                  file.path(name, 
                            list.files(path = file.path(data_dir, name),
                                       pattern="200012.nc")))
}

r <- stack(files)

# crop raster layers to bounded box of USA
e <- as(extent(-124.848974, -66.885444, 24.396308, 49.384358), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
usa <- crop(r, e)

# take a random sample of only 1% of the data
usa.sample <- as.data.frame(sampleRandom(usa, size=(dim(usa)[1]*dim(usa)[2]*0.01),
                                         xy=TRUE, na.rm=TRUE))

# perform k-means clustering, store cluster id in data frame
usa.sample <- data.frame(usa.sample, k = kmeans(usa.sample, 7)$cluster)

# plot data points where color is determined by cluster
plot(usa.sample$x, usa.sample$y, col = usa.sample$k.2,
     xlab = "Latitude", ylab = "Longitude",
     main = "K-Means Clustering for 2000 (using all components)",
     cex.main=0.8, cex.lab = 0.7, cex.axis = 0.6)


