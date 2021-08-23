# Visualization of Aerosol Optical Depth Data
# Author: Nick Wibert
# Last Modified: 08/22/21


# Libraries ---------------------------------------------------------------

library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(raster)
library(rgdal)
library(ggplot2)


# Accessing data from netCDF files ----------------------------------------

# set path and filename
ncpath <- "C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data/"
ncname <- "V4NA03_PM25_NA_201801_201812-RH35"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "PM25" 

ncin <- nc_open(ncfname)
print(ncin)

# get longitude
lon <- ncvar_get(ncin,"LON")
nlon <- dim(lon)
head(lon)

# get latitude
lat <- ncvar_get(ncin,"LAT")
nlat <- dim(lat)
head(lat)

# print dimensions of dataset
print(c(nlon,nlat))


# get particulate matter variable (PM25) and attributes
PM_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(PM_array)

PM_array[PM_array==fillvalue$value] <- NA

title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")


# close netCDF file
nc_close(ncin)



# Visualization -----------------------------------------------------------

year <- 2000
filename <- paste("data/V4NA03_PM25_NA_", year, '01_', year, '12-RH35.nc', sep="")
r <- stack(raster(filename, varname = "PM25"))

for (year in 2001:2018)
{
  filename <- paste("data/V4NA03_PM25_NA_", year, '01_', year, '12-RH35.nc', sep="")
  
  r <- stack(r, raster(filename, varname = "PM25"))
}

names(r) <- c(2000:2018)

plot(r,1:6, breaks=c(seq(0,50,5)), col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")
plot(r,7:12, breaks=c(seq(0,50,5)), col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")
plot(r,13:19, breaks=c(seq(0,50,5)), col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")


# Now do the same for the individual components of PM (SO, NO, NH, DUST, etc.)