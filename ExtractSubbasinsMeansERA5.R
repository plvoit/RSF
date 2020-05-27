## This script extract values from a NetCDF-file (3d-array of lat,lon,time) containing precipitation values at
## given polygons. The mean cell value for each polygone is calculated and concanated 
## to a timeseries for each subbasin. Because
## this script provided the data for CheckERA5-script and is now used to extract
## the data for the subbasins for WASA-SED


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
library(RNetCDF)
library(raster)
library(rgdal)
library(foreach)
library(doParallel)

## create a list of all netcdf files in folder (one for each year)
all_files <- dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/Test", full.names = TRUE)
all_files_short <- dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/Test")

## stack all the netcdf files into one raster stack. One raster per timestep (day)
ncdf_stack <- stack(all_files)

# load shapefile of subbasins for extraction
subbasins <- readOGR("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/GIS/78subbas/78SubbasinsWGS84.shp")

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

## bind all the rows together
DF_mean <- as.data.frame(do.call(rbind, result_list))

colnames(DF_mean) <- subbasins@data$DN

## create date vector for dataframe
start <- as.POSIXct("01.01.2001", format = c("%d.%m.%Y"), tz = "UTC")
end <- as.POSIXct("31.12.2002", format = c("%d.%m.%Y"), tz = "UTC")
time_vector <- seq(start,end, by ="day")
DF_mean$Date <- time_vector


#write.csv(ts, file = "ERA51981_2019_precipitation_at_stations.txt", row.names = FALSE)



#######################################################################################
## test if it works, using the point data from CheckERA5 and the stations from Gerd

Pera <- read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/ERA51981_2019_precipitation_at_stations.txt", check.names = FALSE)
stations_gerd <- readOGR("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/GIS/Stations Gerd/stations_gerd.shp")

Pera$Date <- as.POSIXct(Pera$Date)


extract_ncdf <- function(raster){
  r.vals <- raster::extract(raster, stations_gerd)
  r.mean <- sapply(r.vals, FUN=mean)
  return(r.mean)
}
# extract raster values for each shape feature and calculate the mean for each 
# timestep. Create dataframe with one column for every feature
# Using parallelization saves 58% of the time. Data extraction from the rasters takes quite long
# using this approach 10 years of daily rasters will need about 1h 45 min to be processed

ptm <- proc.time()           #start timer for code block
registerDoParallel(3)
result_list <- foreach(dat=as.list(ncdf_stack)) %dopar% {
  extract_ncdf(dat)
}
stopImplicitCluster()
proc.time()-ptm                #end timer for code block

## bind all the rows together
DF_mean <- as.data.frame(do.call(rbind, result_list))

colnames(DF_mean) <- stations_gerd@data$id

## create date vector for dataframe
start <- as.POSIXct("01.01.2001", format = c("%d.%m.%Y"), tz = "UTC")
end <- as.POSIXct("31.12.2002", format = c("%d.%m.%Y"), tz = "UTC")
time_vector <- seq(start,end, by ="day")
DF_mean$Date <- time_vector

Pera <- Pera[Pera$Date >= "2001-01-01",]
Pera <- Pera[Pera$Date <= "2002-12-31",]

plot(DF_mean$`110`~DF_mean$Date, type = "l")
lines(Pera$`110`~DF_mean$Date, type = "l", col = "red")


plot(DF_mean$`170`~DF_mean$Date, type = "l")
lines(Pera$`170`~DF_mean$Date, type = "l", col = "red")

# the plots are identical the code works
