### This script is just a try to extract multpliple variables (tp,ssrd,tmean) from a netcdf File
### Maybe the data extraction for WASA could be sped up like this
## It looks like you can just extract one variable at a time by nc_var_get. Also it doesn't
## seem to be possible make a brick or stack from a netcdf that contains various variables since it doesn't know
## which one to pick / or just picks the first one
## Then the solution would still be to loop through all the variables and extract each one by itself...



rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
#library(RNetCDF)
library(raster)
#library(ncdf.tools)
library(PaulsPack)
library(sp)
library(rgdal)
library(doParallel)
library(foreach)
## Join the commplete observations with the table that contains station coordinates
## restulting in table wth station ID and coordinates. Will be used to extract from netcdf files

## create a list of all netcdf files in folder (one for each year)
all_files <- as.list(dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/Test", full.names = TRUE))

ncFile <- nc_open( all_files[[1]] )

subbasin <- readOGR("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/Data/ERA5-RAW/testshape/testshape.shp")


print(ncFile)

teststack <- stack(all_files[[1]])

ncvar_get()


# function for data extraction
extract_ncdf <- function(raster){
  r.vals <- raster::extract(raster, subbasin)
  r.mean <- sapply(r.vals, FUN=mean)
  return(r.mean)
}

# extract raster values for each shape feature and calculate the mean for each 
# timestep. Create dataframe with one column for every feature
# Using parallelization saves 58% of the time. Data extraction from the rasters takes quite long
# using this approach 10 years of daily rasters will need about 1h 45 min to be processed

ptm <- proc.time()           #start timer for code block
registerDoParallel(3)
result_list <- foreach(dat=as.list(ncFile)) %dopar% extract_ncdf(dat)   # this is the parallelization bit
stopImplicitCluster()
proc.time()-ptm                #end ti



#for (i in 1:length(all_files)){
#  ncFile <- nc_open( all_files[[i]] )
#  LonIdx <- which( ncFile$dim$lon$vals > min_lon & ncFile$dim$lon$vals < max_lon)
#  LatIdx <- which( ncFile$dim$lat$vals > min_lat & ncFile$dim$lat$vals < max_lat)
#  rainfl <- ncvar_get( ncFile, "Rainf")[ LatIdx,LonIdx, ]
# nc_close(ncFile)
#  array_list[[i]] <- rainfl

#}


## merge all the netcdf files into one raster brick takes about 12 hours...
### extract stations of each file

#timeseries_list <- list()
#for ( i in 1:length(all_files)){
#b <- brick(all_files[[i]])
#station_series <- extract(b, cbind(stations_new$lon, stations_new$lat))
#timeseries_list[[i]] <- station_series
#}

#Save workspace to save time for future runs
#save.image(file = "workspace_NetCDFWatchERA5.RData")

#Restore the workspace
load("workspace_NetCDFWatchERA5.RData")

## transpose extracted timeseries because columns should be rows and vice versa and create a dataframe with date column

timeseries_list <- lapply(timeseries_list,t)

# put all together
stations_df <- do.call(rbind, timeseries_list)


### find the right dates, dates in the netcdf files are given by hours since 01.01.1900
time_vectors <-  list()
for (i in 1:length(all_files)){
  ncfile <- nc_open(all_files[[i]])
  time_string <- ncvar_get(ncfile, "time")
  time_string <- as.POSIXct(time_string*3600,origin = "1900-01-01" ) # found on stackoverflow, seems to work
  time_vectors[[i]] <- time_string
}

## Put all the vectors into one
time_joined <- Reduce(c,time_vectors)

## Create dataframe of the observations with date and ID's as column name
stations_df <- as.data.frame(stations_df)
colnames(stations_df) <- stations_new$id
stations_df  <- cbind(time_joined,stations_df)


## aggregate to daily values ### since WatchERA5 gives Rainfall Flux in kg/m^2s the mean has to be taken
stations_daily <- aggregate_by_time(stations_df, c(2:ncol(stations_df)),"1 day", mean, right = FALSE)
colnames(stations_df)[1] <- "Date"
write.csv(stations_daily, file = "WatchERA51979_2016_precipitation_at_stations.txt", row.names = FALSE)
