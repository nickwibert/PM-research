# Visualization of Aerosol Optical Depth Data
# Author: Nick Wibert
# Last Modified: 08/31/21


# Libraries ---------------------------------------------------------------

library(ncdf4)
library(RColorBrewer)
library(raster)


# Accessing data from netCDF files ----------------------------------------

## NOTE: Make sure your current working directory is the 'PM-research'
## repository before continuing.

# set path and filename
ncpath <- paste(getwd(), "/data/PM25/", sep="")
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

# Let's visualize all the data for PM25 in North America for 2000-2018
# Run the loop and plot code below to plot all 19 years separately.

year <- 2000
filename <- paste("data/PM25/V4NA03_PM25_NA_", year, '01_', year,
                  '12-RH35.nc', sep="")
r <- stack(raster(filename, varname = "PM25"))

for (year in 2001:2018)
{
  filename <- paste("data/PM25/V4NA03_PM25_NA_", year, '01_', year,
                    '12-RH35.nc', sep="")
  
  r <- stack(r, raster(filename, varname = "PM25"))
}

names(r) <- c(2000:2018)

plot(r,1:6, breaks=c(seq(0,50,5)), col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")
plot(r,7:12, breaks=c(seq(0,50,5)), col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")
plot(r,13:18, breaks=c(seq(0,50,5)), col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")


## The following code will do the same, but for a specific component of particulate
## matter. Choose a component by un-commenting ONE of the lines directly below:

comp <- "SO4"  # sulfate
# comp <- "NIT"  # nitrate
# comp <- "NH4"  # ammonium
# comp <- "OM"   # organic matter
# comp <- "BC"   # black carbon
# comp <- "SOIL" # mineral dust
# comp <- "SS"   # sea-salt

year <- 2000

ncpath <- paste(getwd(), "/data/", comp, "/", sep="")
ncname <- paste("GWRwSPEC_", comp, 'p_NA_',
                year, '01_', year, '12-wrtSPECtotal', sep="")
ncfname <- paste(ncpath, ncname, ".nc", sep="")


r <- stack(raster(ncfname, varname = comp))

for (year in 2001:2017)
{
  ncname <- paste("/GWRwSPEC_", comp, 'p_NA_',
                  year, '01_', year, '12-wrtSPECtotal', sep="")
  ncfname <- paste(ncpath, ncname, ".nc", sep="")
  
  r <- stack(r, raster(ncfname, varname = comp))
}

names(r) <- c(2000:2017)

# set min and max for all raster layers
r <- setMinMax(r)

# maxValue returns a vector with the max for each individual
# layer (year in our context). Taking the max of that vector will give us
# the max across all years in the raster, so that we can set a reasonable
# scale of values across all plots for easy comparison from 0 to max.
max <- max(maxValue(r))

plot(r,1:6, breaks=c(seq(from=0,to=max,by=floor(max/10))),
     col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")
plot(r,7:12, breaks=c(seq(from=0,to=max,by=floor(max/10))),
     col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")
plot(r,13:18, breaks=c(seq(from=0,to=max,by=floor(max/10))),
     col=rev(brewer.pal(11,"RdYlBu")), 
     xlab="Longitude", ylab="Latitude")



# Visualization functions -------------------------------------------------

# The function below is pretty much the same the previous section,
# but it allows you to run the function from the command-line
# and choose which component you want to visualize instead of
# manually stepping through the code.

PMvizInteractive <- function()
{
  while (TRUE)
  {
    input <- menu(c("Vizualize Surface PM Data", "Exit"),
                  title = "What would you like to do?")
    
    if (input == 2) { break } # end function
    
    if (input == 1)
    {
      # store vector of composition codes to be used when opening files later
      comp <- c("PM25", "SO4", "NIT", "NH4", "OM", "BC", "SOIL", "SS")
      
      # store vector of years
      years <- c(2000:2017)
      
      cat <- menu(c("PM25 (total particulate matter)", "SO4  (sulfate)",
                    "NIT  (nitrate)", "NH4  (ammonium)",
                    "OM   (organic matter)", "BC   (black carbon)",
                    "SOIL (mineral dust)", "SS   (sea-salt)"),
                  title = "Which data do you want to visualize?")
      
      start <- menu(c(years, "All years"),
                    title = "Starting with which year?")
      
      if (start == 19) # all years
      {
        start <- 1
        end <- 18
      }
      else
      {
        while(TRUE)
        {
          end <- menu(years,
                      title = paste("Ending with which year?",
                      "(choose same year if you only want to plot one)"))
          if (start > end)
          {
            message("ERROR: end year cannot be earlier than start year.")
            message("Please select a different end year.")
            message(paste("Your selected start year is:", years[start]))
          }
          else {break}
        }
      }
      
      message("Gathering data, please wait...")
      
      # PM25 data has a unique filename, so we handle it alone here
      if (cat == 1)
      {
        year <- years[start]
        filename <- paste("data/PM25/V4NA03_PM25_NA_", year, '01_', year,
                          '12-RH35.nc', sep="")
        r <- stack(raster(filename, varname = "PM25"))
        
        if (start != end) # if start and end are the same, skip this
        {
          for (year in years[start+1]:years[end])
          {
            filename <- paste("data/PM25/V4NA03_PM25_NA_", year, '01_', year,
                              '12-RH35.nc', sep="")
            
            r <- stack(r, raster(filename, varname = "PM25"))
          }
        }
      }
      else # the rest of the data has similar filenames
      {
        # comp[cat] will pull the code for the selected composition
        # so that the correct file is read (ex. comp[2] = "SO4")
        
        year <- start
        filename <- paste("data/", comp[cat], "/GWRwSPEC_", comp[cat], 'p_NA_',
                          year, '01_', year, '12-wrtSPECtotal.nc', sep="")
        r <- stack(raster(filename, varname = comp[cat]))
        
        if (start != end) # if start and end are the same, skip this
        {
          for (year in years[start+1]:years[end])
          {
            filename <- paste("data/", comp[cat], "/GWRwSPEC_", comp[cat], 'p_NA_',
                              year, '01_', year, '12-wrtSPECtotal.nc', sep="")
            
            r <- stack(r, raster(filename, varname = comp[cat]))
          }
        }
      }
    
      names(r) <- paste("PM25 in", c(years[start]:years[end]))
      
      plot_count <- length(c(years[start]:years[end]))
      
      # set min and max for all raster layers
      # NOTE: this part can take a while, but is necessary to have 
      # comparable color intervals across plots.
      r <- setMinMax(r)
      
      # maxValue returns a vector with the max for each individual
      # layer (year in our context). Taking the max of that vector will give us
      # the max across all years in the raster, so that we can set a reasonable
      # scale of values across all plots for easy comparison from 0 to max.
      max <- max(maxValue(r))
      
      message("Plotting data...")
      
      if (plot_count < 6)
      {
        plot(r,1:plot_count, breaks=c(seq(0,50,5)),
             col=rev(brewer.pal(11,"RdYlBu")), 
             xlab="Longitude", ylab="Latitude")
      }
      else if (plot_count < 12)
      {
        plot(r,1:6, breaks=c(seq(from=0,to=max,by=floor(max/10))),
             col=rev(brewer.pal(11,"RdYlBu")), 
             xlab="Longitude", ylab="Latitude")
        plot(r,7:plot_count, breaks=c(seq(from=0,to=max,by=floor(max/10))),
             col=rev(brewer.pal(11,"RdYlBu")), 
             xlab="Longitude", ylab="Latitude")
      }
      else
      {
        plot(r,1:6, breaks=c(seq(from=0,to=max,by=floor(max/10))),
             col=rev(brewer.pal(11,"RdYlBu")), 
             xlab="Longitude", ylab="Latitude")
        plot(r,7:12, breaks=c(seq(from=0,to=max,by=floor(max/10))),
             col=rev(brewer.pal(11,"RdYlBu")), 
             xlab="Longitude", ylab="Latitude")
        plot(r,13:plot_count, breaks=c(seq(from=0,to=max,by=floor(max/10))),
             col=rev(brewer.pal(11,"RdYlBu")), 
             xlab="Longitude", ylab="Latitude")
      }
      message("Done!")
    }
  }
}


# This function is the same as the previous function,
# but takes in the pollutant type and years as arguments.
# Much faster as long as you know the pollutant codes.
PMviz <- function(type, years)
{
  # store vector of composition codes
  comp <- c("PM25", "SO4", "NIT", "NH4", "OM", "BC", "SOIL", "SS")
  
  # store vector of years
  if (missing(years))
  { years <- c(2000:2017) }
  
  # argument error-checking
  if (!(type %in% comp))
  {
    message("ERROR: Invalid pollutant type. Use one of the following codes:")
    print(c("PM25 (total particulate matter)","SO4  (sulfate)",
            "NIT  (nitrate)", "NH4  (ammonium)",
            "OM   (organic matter)", "BC   (black carbon)",
            "SOIL (mineral dust)", "SS   (sea-salt)"))
    invokeRestart("abort")
  }
  
  message("Gathering data, please wait...")
    
  # PM25 data has a unique filename, so we handle it alone here
  if (type == "PM25")
  {
    year <- years[1]
    filename <- paste("data/PM25/V4NA03_PM25_NA_", year, '01_', year,
                      '12-RH35.nc', sep="")
    r <- stack(raster(filename, varname = "PM25"))
    
    if (length(years) > 1) # if start and end are the same, skip this
    {
      for (year in years[2:length(years)])
      {
        filename <- paste("data/PM25/V4NA03_PM25_NA_", year, '01_', year,
                          '12-RH35.nc', sep="")
        
        r <- stack(r, raster(filename, varname = "PM25"))
      }
    }
  }
  else # the rest of the data has similar filenames
  {
    year <- years[1]
    filename <- paste("data/", type, "/GWRwSPEC_", type, 'p_NA_',
                      year, '01_', year, '12-wrtSPECtotal.nc', sep="")
    r <- stack(raster(filename, varname = type))
    
    if (length(years) > 1) # if start and end are the same, skip this
    {
      for (year in years[2:length(years)])
      {
        filename <- paste("data/", type, "/GWRwSPEC_", type, 'p_NA_',
                          year, '01_', year, '12-wrtSPECtotal.nc', sep="")
        
        r <- stack(r, raster(filename, varname = type))
      }
    }
  }

  names(r) <- paste("PM25 in", years)
  
  plot_count <- length(years)
  
  # set min and max for all raster layers
  # NOTE: this part can take a while, but is necessary to have 
  # comparable color intervals across plots.
  r <- setMinMax(r)
  
  # maxValue returns a vector with the max for each individual
  # layer (year in our context). Taking the max of that vector will give us
  # the max across all years in the raster, so that we can set a reasonable
  # scale of values across all plots for easy comparison from 0 to max.
  max <- max(maxValue(r))
  
  message("Plotting data...")
  
  if (plot_count < 6)
  {
    plot(r,1:plot_count, breaks=c(seq(0,50,5)),
         col=rev(brewer.pal(11,"RdYlBu")), 
         xlab="Longitude", ylab="Latitude")
  }
  else if (plot_count < 12)
  {
    plot(r,1:6, breaks=c(seq(from=0,to=max,by=floor(max/10))),
         col=rev(brewer.pal(11,"RdYlBu")), 
         xlab="Longitude", ylab="Latitude")
    plot(r,7:plot_count, breaks=c(seq(from=0,to=max,by=floor(max/10))),
         col=rev(brewer.pal(11,"RdYlBu")), 
         xlab="Longitude", ylab="Latitude")
  }
  else
  {
    plot(r,1:6, breaks=c(seq(from=0,to=max,by=floor(max/10))),
         col=rev(brewer.pal(11,"RdYlBu")), 
         xlab="Longitude", ylab="Latitude")
    plot(r,7:12, breaks=c(seq(from=0,to=max,by=floor(max/10))),
         col=rev(brewer.pal(11,"RdYlBu")), 
         xlab="Longitude", ylab="Latitude")
    plot(r,13:plot_count, breaks=c(seq(from=0,to=max,by=floor(max/10))),
         col=rev(brewer.pal(11,"RdYlBu")), 
         xlab="Longitude", ylab="Latitude")
  }
  message("Done!")
}
