## This script extract values from a NetCDF-file (3d-array of lat,lon,time) containing precipitation values at
## given spatial points
## this script provided the data for CheckERA5-script


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
library(RNetCDF)
library(raster)

## Join the commplete observations with the table that contains station coordinates
## restulting in table wth station ID and coordinates. Will be used to extract from netcdf files

stations <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/meta.txt", comment.char="#")
obs_complete <- read.csv("~/Workspace/RioSaoFrancisco/xds_gerd/xds_gerd/brazil/era5/P.csv", check.names = FALSE)
obs_complete <- colnames(obs_complete[,4:ncol(obs_complete)])
stations_new <- stations[stations$id %in% obs_complete,]
stations_new <- stations_new[stations_new$varname == "precipitation",]

## create a list of all netcdf files in folder (one for each year)

all_files <- as.list(dir("~/Workspace/RioSaoFrancisco/ERA5", full.names = TRUE))


## merge all the netcdf files into one, this takes very long
#b <- brick(all_files)

## extract timeseries for stations
ts <- extract(b, cbind(stations_new$lon, stations_new$lat))

#Save workspace to save time for future runs
#save.image(file = "workspace_NetCDF.RData")

#Restore the workspace
load("workspace_NetCDF.RData")


## transpose extracted timeseries because columns should be rows and vice versa and create a dataframe with date column

ts <- t(ts)
rownames(ts) <- c()  # delete rownames
ts <- as.data.frame(ts)
colnames(ts) <- stations_new$id

## create date vector for dataframe
start <- as.POSIXct("01.01.1981", format = c("%d.%m.%Y"), tz = "UTC")
end <- as.POSIXct("31.12.2019", format = c("%d.%m.%Y"), tz = "UTC")
time_vector <- seq(start,end, by ="day")

ts$Date <- time_vector
ts <- ts[,c(65,1:64)]

write.csv(ts, file = "ERA51981_2019_precipitation_at_stations.txt", row.names = FALSE)
