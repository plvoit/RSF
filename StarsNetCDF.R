## Since there was an error in the old SSRD datase, a new SSRD had to be downloaded from Copernicus.
## this data wasn't preprocessed. Using the other ERA5-Netcdf scripts would have been to slow, due to the 
## size of the raw files. 
## Jose suggsted to use the new stars package. With this already before opening, a subset (bounding box) can be defined
## This process helps a lot to speed up the data extraction.
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
library(RNetCDF)
library(raster)
library(rgdal)
library(foreach)
library(doParallel)
library(stars)
library(sf)

all_files <- dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/SSRDNew", full.names = TRUE)  #put here the path  to the NetCDF-Files
all_files_short <- dir("~/Workspace/RioSaoFrancisco/Data/ERA5-RAW/SSRDNew")

# load shapefile of subbasins for extraction
subbasins <- readOGR("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/GIS/75subbas-neu/Shape/75SubbasWGS84.shp")

# function for data extraction
extract_ncdf <- function(raster){
  r.vals <- raster::extract(raster, subbasins)
  r.mean <- sapply(r.vals, FUN=mean)
  return(r.mean)
}

DF <- data.frame()
ptm <- proc.time()           #start timer for code block
for (i in 1: length(all_files)){

  # get lat and lon vals from netcdf  
nc <- nc_open(all_files[i])
  y=nc$dim$lat$vals
  x=nc$dim$lon$vals
  demo_lat=which(y > -21 & y < -7)
  demo_lon=which(x > -48 & x < -35)
  z <- length(nc$dim$time$vals)
  nc_close(nc)
 
  # read and subset netcdf with stars package
  file <- read_ncdf(all_files[i],var='SWdown',ncsub=cbind(start=c(demo_lon[1],demo_lat[1],1),count=c(length(demo_lon),length(demo_lat),z)))

  # aggregate to daily resolution
  file <- aggregate(file,"1 day", FUN = mean)

  # transform stars object to raster
  file <- as(file, "Raster")

  # raster extent got lost on the way, set it again
  file <- setExtent(file,c(min(x[demo_lon]),max(x[demo_lon]),min(y[demo_lat]),max(y[demo_lat])))

# extract raster values for each shape feature and calculate the mean for each 
# timestep. Create dataframe with one column for every feature
# Using parallelization saves 58% of the time. Data extraction from the rasters takes quite long
# using this approach 10 years of daily rasters will need about 1h 45 min to be processed


registerDoParallel(3)
result_list <- foreach(dat=as.list(file)) %dopar% extract_ncdf(dat)   # this is the parallelization bit
stopImplicitCluster()


## bind all the rows together
dummy <- as.data.frame(do.call(rbind, result_list))

colnames(dummy) <- subbasins@data$DN

DF <- rbind(DF,dummy)

}
proc.time()-ptm   
#Save workspace to save time for future runs
#save.image(file = "workspace_StarsNetCDF.RData")

## create date vector for dataframe ###!!!! this could be done more elegant as done in script getNCEP 
start <- as.POSIXct("01.01.2000", format = c("%d.%m.%Y"), tz = "UTC")
end <- as.POSIXct("31.12.2009", format = c("%d.%m.%Y"), tz = "UTC")
time_vector <- seq(start,end, by ="day")
DF$Date <- time_vector

###Format the file for WASA
input_list <-  list(DF)

# make vector with the order of the list for later naming the individual files

naming_vector <- c("radiation")

# change the order of columns and format the date
input_list <- lapply(input_list, function(x) x <- x[,c("Date",names(x))])
input_list <- lapply(input_list, function(x) { x["Date.1","999"] <- NULL; x })  # ,x within function seems to be the return value, otherwise you'd get nothing
input_list <- lapply(input_list, function(x) {x [,"Date"] <- as.POSIXct(x[,"Date"], format = c("%Y-%m-%d")); x} )

## change the format of the date as it's wanted for WASA
input_list <- lapply(input_list, function(x) { x[,"Date"] <- format(x[,"Date"], "%d%m%Y") ;x } )

# add number of days column, the timeseries are in daily resolution
input_list <- lapply(input_list, function(x) {x["No. of days"] <- 1:nrow(x) ; x })

# change the order of the columns
input_list <- lapply(input_list, function(x) x <- x[,c("Date","No. of days", names(x))])
input_list <- lapply(input_list, function(x) { x[c("Date.1","No. of days.1")] <- NULL; x })

input_list <- lapply(input_list, function(x) {names(x)[c(1,2)] <- c("0","0"); x })

###round
input_list[[1]][2:ncol(input_list[[1]])] <- round(input_list[[1]][2:ncol(input_list[[1]])],1)

# save all the files in a folder. The header will be added manually since it's just four timeseries files

for (i in 1:length(input_list)){
  write.table(input_list[[i]],file = paste(naming_vector[i],".dat", sep = ""), sep = "\t", row.names = FALSE, quote = FALSE)
  
}
## still need to add header manually