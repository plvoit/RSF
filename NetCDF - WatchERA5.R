## This script extract values from a NetCDF-file (3d-array of lat,lon,time) containing precipitation values at
## given spatial points
## this script provided the data for CheckWatchERA5-script
## WatchERA5 data comes in worldwide raster. This needs some preprocessing to handle the file size


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
#library(RNetCDF)
library(raster)
#library(ncdf.tools)
library(PaulsPack)

## Join the commplete observations with the table that contains station coordinates
## restulting in table wth station ID and coordinates. Will be used to extract from netcdf files

stations <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/meta.txt", comment.char="#")
obs_complete <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/era5/P.csv", check.names = FALSE)
obs_complete <- colnames(obs_complete[,4:ncol(obs_complete)])
stations_new <- stations[stations$id %in% obs_complete,]
stations_new <- stations_new[stations_new$varname == "precipitation",]

## create a list of all netcdf files in folder (one for each year)
all_files <- as.list(dir("~/Workspace/RioSaoFrancisco/Data/WatchERA5", full.names = TRUE))



### this works to create one big array just for the extend of brasil but will loose coordinates in the process.
### Basically just subsetting the wordlwide WatchERA-files

## check extend of the ERA5 files to subset from WatchERA data
# era5 <- nc_open("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/Data/ERA5/ERA5_Land_daily_tp_1981_SF_Basin.nc")
# #print(era5)
# lon <- ncvar_get(era5,"lon")
# lat <- ncvar_get(era5,"lat")
# min_lon <- min(lon)
# min_lat <- min(lat)
# max_lon <- max(lon)
# max_lat <- max(lat)
# 
# array_list <- list()

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
