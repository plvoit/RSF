## This script extract values from a NetCDF-file (3d-array of lat,lon,time) containing precipitation values at
## given polygons. The mean cell value for each polygone is calculated and concanated 
## to a timeseries for each subbasin
## this script provided the data for CheckERA5-script


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
library(RNetCDF)
library(raster)
library(rgdal)


## create a list of all netcdf files in folder (one for each year)

all_files <- dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/Test", full.names = TRUE)
all_files_short <- dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/Test")

## stack all the netcdf files into one raster stack
ncdf_stack <- stack(all_files)

#Save workspace to save time for future runs
#save.image(file = "workspace_NetCDF.RData")
#Restore the workspace
#load("workspace_NetCDF.RData")

# load shapefile of subbasins for extraction
subbasins <- readOGR("C:/Users/Admin/Documents/QGIS/Neuer Ordner/testshape.shp")


DF_mean <- data.frame()

for ( i in 1:length(ncdf_stack@layers)){
# Extract raster values to list object
r.vals <- extract(subset(ncdf_stack,i), subbasins)

# Use (simple)list apply to calculate mean for each polygon, stored in a vector
r.mean <- sapply(r.vals, FUN=mean)
DF_mean <- rbind(DF_mean, r.mean)
}
colnames(DF_mean) <- subbasins@data$id

## create date vector for dataframe
start <- as.POSIXct("01.01.2001", format = c("%d.%m.%Y"), tz = "UTC")
end <- as.POSIXct("31.12.2010", format = c("%d.%m.%Y"), tz = "UTC")
time_vector <- seq(start,end, by ="day")
DF_mean$Date <- time_vector


## check if extracted timeseries are right compared the ones from before. Yes, it looks fine

# Pera <- read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/ERA51981_2019_precipitation_at_stations.txt", check.names = FALSE)
# Pera$Date <- as.POSIXct(Pera$Date)
# 
# plot(DF_mean$`102`~DF_mean$Date, type = "l")
# lines(Pera$`102`~Pera$Date, type = "l", col = "red")
# 
# plot(DF_mean$`232`  ~DF_mean$Date, type = "l")
# lines(Pera$`232` ~Pera$Date, type = "l", col = "red")
# 
# plot(DF_mean$`368` ~DF_mean$Date, type = "l")
# lines(Pera$`368`~Pera$Date, type = "l", col = "red")


write.csv(ts, file = "ERA51981_2019_precipitation_at_stations.txt", row.names = FALSE)
