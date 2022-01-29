# K-Means Clustering of Aerosol Optical Depth Data
# Author: Nick Wibert
# Last Modified: 10/26/21


# Libraries ---------------------------------------------------------------

library(stats)
library(ncdf4)
library(raster)
library(sf)
library(rgdal)
library(ggplot2)
library(tigris)
library(tidyverse)
library(rgeos)
library(spatialEco)
library(ptinpoly)
library(RColorBrewer)

# K-means clustering ------------------------------------------------------

data_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/Annual"

years <- 2000:2016
names <- c("BC", "NH4", "NIT", "OM", "SO4", "SOIL", "SS")

usa <- readOGR("C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/cb_2018_us_nation_5m.shp")
# crop to lower 48 states
e <- as(extent(-125, -66, 24, 50), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
usa <- crop(usa,e)

#1. coerce to points, then to data frame
# names are Lines.NR Lines.ID Line.NR coords.x1 coords.x2
## start here for polys
lin <- as(usa, "SpatialLinesDataFrame")  
## start here for lines
pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))


#3. convert to data frame
dpts <- ggplot2::fortify(usa)

x.usa <- dpts$long  
y.usa <- dpts$lat

for (year in years)
{
  files <- c()
  # get annual data for all components in the current year
  for (name in names)
  {
    files <- append(files,
                    file.path(data_dir,name, 
                              list.files(path = file.path(data_dir, name),
                                         pattern=paste(year, "12.nc", sep=""))))
  }
  
  r <- stack(files)
  
  # crop raster layers to bounded box of USA
  e <- as(extent(-124.848974, -66.885444, 24.396308, 49.384358), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  r <- crop(r, e)
  
  # take a random sample of only 1% of the data
  r.sample <- as.data.frame(sampleRandom(r, size=(dim(r)[1]*dim(r)[2]*0.01),
                                           xy=TRUE, na.rm=TRUE))
  
  x <- r.sample[,1]
  y <- r.sample[,2]
  
  xy <- data.frame(x,y)
  coordinates(xy) = ~x + y
  proj4string(xy) = proj4string(usa)
  
  ov <- over(xy, as(usa,"SpatialPolygons"))
  
  r.sample <- data.frame(r.sample, inUSA = ov)
  
  r.final <- r.sample[complete.cases(r.sample),]
  
  # perform k-means clustering, store cluster id in data frame
  usa.kmeans <- kmeans(r.final[,-c(1,2,10)], 5)
  
  # plot data points where color is determined by cluster
  plot(r.final$x, r.final$y, col = usa.kmeans$cluster,
       xlab = "Latitude", ylab = "Longitude",
       main = paste("K-Means Clustering for ", year, sep=""),
       pch=1, cex.main=0.8, cex.lab = 0.7, cex.axis = 0.6, cex=0.8)
  
  lines(usa)
  legend(-126, 35, legend=sort(unique(usa.kmeans$cluster)),
         col = sort(unique(usa.kmeans$cluster)), pch = 19, cex = 0.7)
  
  # Heatmap
  centers <- usa.kmeans$centers
  centers <- cbind(centers, rowSums(centers))
  colnames(centers)[8] <- "Total"
  
  centers.df <- as.data.frame(as.table(centers))
  names(centers.df) <- c("Cluster", "Name", "Value")
  
  print(
    ggplot(centers.df, aes(x = Cluster, y = Name, fill = Value)) + 
    geom_tile() + ggtitle(paste("Cluster Centers for ", year, sep="")) +
    scale_y_discrete(limits=rev(levels(centers.df$Name))) +
    theme(axis.title.x=element_text(),
          axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5, face="bold")) +
    scale_fill_distiller(palette = 5, direction = 1) +
    labs(fill = expression(paste(mu, "g/", m^3)))
  )
  
  rm(list=c("files", "r.sample", "r.final", "centers", "r", "ov", "usa.kmeans", "xy"))
}


## create heatmaps using .dat files saved
## from jobs run on HiPerGator
data_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/Rdata/"

files <- list.files(data_dir, ".dat")

for (file in files)
{
  load(file.path(data_dir, file))
  
  year <- substr(file, 10, 13)
  
  centers.df <- as.data.frame(as.table(centers))
  names(centers.df) <- c("Cluster", "Name", "Value")
  
  print(
    ggplot(centers.df, aes(x = Cluster, y = Name, fill = Value)) + 
      geom_tile() + ggtitle(paste("Cluster Centers for ", year, sep="")) +
      scale_y_discrete(limits=rev(levels(centers.df$Name))) +
      theme(axis.title.x=element_text(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, face="bold")) +
      scale_fill_distiller(palette = 5, direction = 1) +
      labs(fill = expression(paste(mu, "g/", m^3)))
  )
  
  # Note: add limits as an argument to scale_fill_distiller
  # if you want to print all heatmaps on the same fixed scale for comparison.
  # You should also remove the "Total" category when you do this,
  # as it skews the scale.
  
  rm(centers)
}




# K-means clustering on differences ---------------------------------------

# Same approach as above, but perform clustering on the
# difference across two different years. For example: 2000 and 2005.
# Get all the components for 2000 and all the components for 2005,
# subtract the former from the latter, then perform K-means clustering
# using the resulting values.

KmeansDiff <- function(startYear, endYear)
{
  data_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/Annual"
  
  names <- c("BC", "NH4", "NIT", "OM", "SO4", "SOIL", "SS")
  
  usa <- readOGR("C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/cb_2018_us_nation_5m.shp")
  # crop to lower 48 states
  e <- as(extent(-125, -66, 24, 50), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  usa <- crop(usa,e)
  
  #1. coerce to points, then to data frame
  # names are Lines.NR Lines.ID Line.NR coords.x1 coords.x2
  ## start here for polys
  lin <- as(usa, "SpatialLinesDataFrame")  
  ## start here for lines
  pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))
  
  
  #3. convert to data frame
  dpts <- ggplot2::fortify(usa)
  
  x.usa <- dpts$long  
  y.usa <- dpts$lat
  
  startFiles <- c()
  endFiles <- c()
  
  # get annual data for all components in startYear and endYear
  for (name in names)
  {
    startFiles <- append(startFiles,
                    file.path(data_dir,name, 
                              list.files(path = file.path(data_dir, name),
                                         pattern=paste(startYear, "12.nc", sep=""))))
    endFiles <- append(endFiles,
                         file.path(data_dir,name, 
                                   list.files(path = file.path(data_dir, name),
                                              pattern=paste(endYear, "12.nc", sep=""))))
  }
  
  r1 <- stack(startFiles)
  r2 <- stack(endFiles)
  
  r <- r2 - r1
  
  names(r) <- names
  
  # crop raster layers to bounded box of USA
  e <- as(extent(-124.848974, -66.885444, 24.396308, 49.384358), 'SpatialPolygons')
  crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
  r <- crop(r, e)
  
  # take a random sample of only 1% of the data
  r.sample <- as.data.frame(sampleRandom(r, size=(dim(r)[1]*dim(r)[2]*0.01),
                                         xy=TRUE, na.rm=TRUE))
  
  x <- r.sample[,1]
  y <- r.sample[,2]
  
  xy <- data.frame(x,y)
  coordinates(xy) = ~x + y
  proj4string(xy) = proj4string(usa)
  
  ov <- over(xy, as(usa,"SpatialPolygons"))
  
  r.sample <- data.frame(r.sample, inUSA = ov)
  
  r.final <- r.sample[complete.cases(r.sample),]
  
  # perform k-means clustering, store cluster id in data frame
  usa.kmeans <- kmeans(r.final[,-c(1,2,10)], 10)
  
  # plot data points where color is determined by cluster
  plot(r.final$x, r.final$y, col = usa.kmeans$cluster,
       xlab = "Latitude", ylab = "Longitude",
       main = paste("K-Means Clustering on Difference:", startYear, "to", endYear),
       pch=1, cex.main=0.8, cex.lab = 0.7, cex.axis = 0.6, cex=0.6)
  
  lines(usa)
  legend(-126, 37, legend=sort(unique(usa.kmeans$cluster)),
         col = sort(unique(usa.kmeans$cluster)), pch = 19, cex = 0.7)
  
  # Heatmap
  centers <- usa.kmeans$centers
  centers <- cbind(centers, rowSums(centers))
  colnames(centers)[8] <- "Total"
  
  centers.df <- as.data.frame(as.table(centers))
  names(centers.df) <- c("Cluster", "Name", "Value")
  
  print(
    ggplot(centers.df, aes(x = Cluster, y = Name, fill = Value)) + 
      geom_tile() + ggtitle(paste("Cluster Centers for Difference:", startYear, "to", endYear)) +
      scale_y_discrete(limits=rev(levels(centers.df$Name))) +
      theme(axis.title.x=element_text(),
            axis.title.y=element_blank(),
            plot.title = element_text(hjust = 0.5, face="bold")) +
      scale_fill_distiller(palette = 5, direction = 1) +
      labs(fill = expression(paste(mu, "g/", m^3)))
  )
  
  rm(list=c("files", "r.sample", "r.final", "centers", "r", "ov", "usa.kmeans", "xy"))
}
