# This script processes NCEP files. Since there are no datasets for relative humidity (yet) from KIT
# for the first run of WASA we'll use NCEP. This data comes in a global dataset as NetCDF-Files.
# Since the projection is different than the ones from ERA5 or WatchERA5 the shapefile of the subbasins 
# needs to be adjusted so it fits with the NCEP raster.
# The script extracts the mean values for the subbasins given by the shapefile.
# NCEP is recorded every six hours. The timestamp of the NetCDF is extracted and then aggregated (mean)
# to daily values

# Copyright (C) 2020 Paul Voit

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
library(RNetCDF)
library(raster)
library(rgdal)
library(foreach)
library(doParallel)
library(PaulsPack)

## create a list of all netcdf files in folder (one for each year)
all_files <- dir("~/Workspace/RioSaoFrancisco/Data/NCEP/Test", full.names = TRUE)
all_files_short <- dir("~/Workspace/RioSaoFrancisco/Data/NCEP/Test")


## stack all the netcdf files into one raster stack. One raster per timestep (day)
ncdf_stack <- stack(all_files)

# load shapefile of subbasins for extraction
subbasins <- readOGR("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/GIS/75subbas-neu/Translated_forNCEP/75subbas_translated.shp")

#Restore the workspace
#load("workspace_NetCDF.RData")

#Save workspace to save time for future runs
#save.image(file = "workspace_NetCDF.RData")

# function for data extraction
extract_ncdf <- function(raster){
  r.vals <- raster::extract(raster, subbasins)
  r.mean <- sapply(r.vals, FUN=mean)
  return(r.mean)
}

# extract raster values for each shape feature and calculate the mean for each 
# timestep. Create dataframe with one column for every feature
# Using parallelization saves 58% of the time. Data extraction from the rasters takes quite long
# using this approach 10 years of daily rasters will need about 1h 45 min to be processed

ptm <- proc.time()           #start timer for code block
registerDoParallel(3)
result_list <- foreach(dat=as.list(ncdf_stack)) %dopar% extract_ncdf(dat)   # this is the parallelization bit
stopImplicitCluster()
proc.time()-ptm                #end timer for code block

#Save workspace to save time for future runs
#save.image(file = "workspace_NCEP.RData")

#Restore the workspace
#load("workspace_NCEP.RData")

## bind all the rows together
DF_mean <- as.data.frame(do.call(rbind, result_list))

colnames(DF_mean) <- subbasins@data$DN


#check how the timestamp is recorded in these NetCDF-Files
ncfile <- nc_open(all_files[[1]])
print(ncfile)
##you can see: units: hours since 1800-01-01 00:00:0.0


### find the right dates, dates in the netcdf files are given by hours since 01.01.1800
time_vectors <-  list()
for (i in 1:length(all_files)){
  ncfile <- nc_open(all_files[[i]])
  time_string <- ncvar_get(ncfile, "time")
  time_string <- as.POSIXct(time_string*3600,origin = "1800-01-01" ) # conversion found on stackoverflow, seems to work
  time_vectors[[i]] <- time_string
}

# Put all the vectors into one
time_joined <- Reduce(c,time_vectors)

# attach timevector to dataframe
DF_mean$Date <- time_joined

## NCEP is 6-hourly needs to be aggregated to daily

DF_mean_daily <- aggregate_by_time(DF_mean,c(1:75),"1 day",mean)

#rename date column
colnames(DF_mean_daily)[1] <-  "Date"

# format the date column
DF_mean_daily$Date <- as.POSIXct(DF_mean_daily$Date)
DF_mean_daily$Date <- as.POSIXct(format(DF_mean_daily$Date, "%Y-%m-%d"))


write.csv(DF_mean_daily, file = "NCEP2000_2009_rHum_at_75subbasins.txt", row.names = FALSE)


#save.image(file = "workspace_NCEP30years.RData")

#load("workspace_NCEP30years.RData")
