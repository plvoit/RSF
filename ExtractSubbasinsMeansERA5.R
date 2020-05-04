## This script extract values from a NetCDF-file (3d-array of lat,lon,time) containing precipitation values at
## given spatial points
## this script provided the data for CheckERA5-script


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
library(RNetCDF)
library(raster)
library(rgdal)

## Join the commplete observations with the table that contains station coordinates
## restulting in table wth station ID and coordinates. Will be used to extract from netcdf files

stations <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/meta.txt", comment.char="#")
obs_complete <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/era5/P.csv", check.names = FALSE)
obs_complete <- colnames(obs_complete[,4:ncol(obs_complete)])
stations_new <- stations[stations$id %in% obs_complete,]
stations_new <- stations_new[stations_new$varname == "precipitation",]

## create a list of all netcdf files in folder (one for each year)

all_files <- as.list(dir("~/Workspace/RioSaoFrancisco/Data/ERA5 - RAW", full.names = TRUE))


## merge all the netcdf files into one, this takes very long
#b <- brick(all_files)

#Save workspace to save time for future runs
save.image(file = "workspace_NetCDF.RData")
# load shapefile for extraction

testshape <- readOGR("C:/Users/Admin/Documents/QGIS/Neuer Ordner/testshape.shp")

#Restore the workspace

load("workspace_NetCDF.RData")

## extract timeseries for stations
ts <- extract(b, testshape)

teststack <-  stack(all_files[[1]])

testbrick <- brick(all_files[[1]])
ts_shape <- extract(testbrick,testshape)


Rasterimage <- raster::mask(subset(testbrick,1),testshape[which(testshape$id == 1),])
image(Rasterimage)

DF_mean <- matrix(ncol=3, nrow =365)

for ( i in 1:365){
# Extract raster values to list object
r.vals <- extract(subset(teststack,i), testshape)

# Use list apply to calculate mean for each polygon
r.mean <- sapply(r.vals, FUN=mean)
DF_mean[i,] <- r.mean
}
colnames(DF_mean) <- testshape@data$id

## create date vector for dataframe
start <- as.POSIXct("01.01.1981", format = c("%d.%m.%Y"), tz = "UTC")
end <- as.POSIXct("31.12.1981", format = c("%d.%m.%Y"), tz = "UTC")
time_vector <- seq(start,end, by ="day")

DF_mean <- as.data.frame(DF_mean)
DF_mean$Date <- time_vector


## check if extracted timeseries are right compared the ones from before. Yes, it looks fine

Pera <- read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/ERA51981_2019_precipitation_at_stations.txt", check.names = FALSE)
Pera$Date <- as.POSIXct(Pera$Date)

plot(DF_mean$V1~DF_mean$Date, type = "l")
lines(Pera$`102`~Pera$Date, type = "l", col = "red")

plot(DF_mean$V2~DF_mean$Date, type = "l")
lines(Pera$`232`~Pera$Date, type = "l", col = "red")

plot(DF_mean$V3~DF_mean$Date, type = "l")
lines(Pera$`368`~Pera$Date, type = "l", col = "red")

# Write results
writeOGR(sdata, getwd(), outshp, driver="ESRI Shapefile", check_exists=TRUE, 
         overwrite_layer=TRUE)


### select single features:
#plot(testshape[which(testshape$id == 1),])






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
