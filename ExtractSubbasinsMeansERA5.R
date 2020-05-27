## This script extract values from a NetCDF-file (3d-array of lat,lon,time) containing precipitation values at
## given polygons. The mean cell value for each polygone is calculated and concanated 
## to a timeseries for each subbasin
## this script provided the data for CheckERA5-script and is now used to extract
## the data for the subbasins for WASA-SED


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
library(RNetCDF)
library(raster)
library(rgdal)
library(pbapply)

## create a list of all netcdf files in folder (one for each year)
all_files <- dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/Test", full.names = TRUE)
all_files_short <- dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/Test")

## stack all the netcdf files into one raster stack. One raster per timestep
ncdf_stack <- stack(all_files)

# load shapefile of subbasins for extraction
subbasins <- readOGR("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/GIS/78subbas/78SubbasinsWGS84.shp")

#Restore the workspace
load("workspace_NetCDF.RData")

#Save workspace to save time for future runs
#save.image(file = "workspace_NetCDF.RData")

# extract raster values for each shape feature and calculate the mean for each 
# timestep. Create dataframe with one column for every feature

DF_mean <- data.frame()

ptm <- proc.time()

for ( i in 1:length(ncdf_stack@layers)){
# Extract raster values to list object
r.vals <- extract(subset(ncdf_stack,i), subbasins)
# Use (simple)list apply to calculate mean for each polygon, stored in a vector
r.mean <- sapply(r.vals, FUN=mean)
DF_mean <- rbind(DF_mean, r.mean)
}

# Stop the clock
proc.time() - ptm


colnames(DF_mean) <- subbasins@data$DN

## create date vector for dataframe
start <- as.POSIXct("01.01.2001", format = c("%d.%m.%Y"), tz = "UTC")
end <- as.POSIXct("31.12.2002", format = c("%d.%m.%Y"), tz = "UTC")
time_vector <- seq(start,end, by ="day")
DF_mean$Date <- time_vector


write.csv(ts, file = "ERA51981_2019_precipitation_at_stations.txt", row.names = FALSE)

ptm <- proc.time()
r.vals <- extract(subset(ncdf_stack,1), subbasins)
proc.time() -ptm


extract_ncdf <- function(raster){
  r.vals <- raster::extract(raster, subbasins)
  r.mean <- sapply(r.vals, FUN=mean)
  return(r.mean)
}




## this option isn't faster
ptm <- proc.time()
library(parallel)
cl <- makeCluster( detectCores()-1 )
clusterExport(cl, "extract_ncdf")
test <- pblapply(as.list(ncdf_stack), extract_ncdf)
stopImplicitCluster()
proc.time()-ptm

## this one is, but result is weird
ptm <- proc.time()
library(foreach)
library(doParallel)
registerDoParallel(3)
test <- foreach(dat=as.list(ncdf_stack)) %dopar% {
  r.vals <- raster::extract(dat, subbasins)
  # Use (simple)list apply to calculate mean for each polygon, stored in a vector
  r.mean <- sapply(r.vals, FUN=mean)
  return(r.mean)
}
stopImplicitCluster()
proc.time()-ptm


## this option works best
### mit Funktion
ptm <- proc.time()
library(foreach)
library(doParallel)
registerDoParallel(3)
test <- foreach(dat=as.list(ncdf_stack)) %dopar% {
extract_ncdf(dat)
}
stopImplicitCluster()
proc.time()-ptm

## bind all the rows together
test_df <- do.call(rbind, test)
