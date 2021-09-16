# Convert netCDF to data.frame
# Author: Nick Wibert
# Last Modified: 09/12/21


# Libraries ---------------------------------------------------------------

library(ncdf4)
library(raster)
library(tidyverse)

# Store data from netCDF file in data.frame -------------------------------

# PM-research directory
data_dir <- "C:/Users/nickw/OneDrive/Desktop/UF/Undergrad Research/PM-research/data"

#names <- c("PM25", "SO4p", "NITp", "NH4p", "OMp", "BCp", "SOILp", "SSp")
#years <- 2000:
names <- "BCp"
years <- 2000:2017
months <- formatC(1:12, width=2, flag="0")

for (name in names)
{
  # create directory for material if it does not exist
  ifelse(!dir.exists(file.path(data_dir, "Monthly", "R data", name)),
         dir.create(file.path(data_dir, "Monthly", "R data", name)), FALSE)
  
  for (year in years)
  {
    # create directory for year if it does not exist
    ifelse(!dir.exists(file.path(data_dir, "Monthly", "R data", name, year)),
           dir.create(file.path(data_dir, "Monthly", "R data", name, year)), FALSE)
    
    for (month in months)
    {
      # data frame name
      df <- paste(name, "_", year, month, sep="")
      
      # name of output file
      output.name <- paste(data_dir, "/Monthly/R data/", name, "/", year, "/", df, ".dat", sep="")
      
      # if output file already exists, continue
      if (file.exists(output.name)) {next;}
      
      
      # If the code reaches this point, a file does not already exist for the given
      # name, year, and month. The code below pulls the data from the .nc file into
      # a raster object, converts that object into a dataframe, and then saves
      # the data frame as a .dat file to the appropriate directory.
      
      
      if (name == "PM25") # PM25 has a uniquely formatted file name
      {
        filename <- paste("V4NA03_PM25_NA_", year, month, "_", year, month, "-RH35.nc", sep="")
      }
      else if (year == 2017) # file name convention changed in 2017 for other materials
      {
        filename <- paste("GWRwSPEC.HEI_", name, "_NA_", year, month, "_",
                          year, month, "-wrtSPECtotal.nc", sep="")
      }
      else
      {
        filename <- paste("GWRwSPEC_", name, "_NA_", year, month, "_",
                          year, month, "-wrtSPECtotal.nc", sep="")
      }
      
      # path to .nc file
      filepath <- file.path(data_dir, "Monthly", "netcdf", name, filename)
      
      # create raster object
      r <- raster(filepath, varname = str_remove(name, "p"))

      
      # convert raster object to data frame and store it as the name created
      # at the beginning of the first loop
      assign(df, as.data.frame(r, xy=TRUE))
      
      # save the data frame to the appropriate directory as a .dat file
      # (file will have the same name as the data frame)
      save(list = df, file = output.name)
      
      # remove raster object and data frame
      rm(r)
      rm(list=df)
    }
  }
}

# Clean-up
rm(list=ls())
