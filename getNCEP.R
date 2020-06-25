rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
library(RNetCDF)
library(raster)
library(rgdal)
library(foreach)
library(doParallel)

## create a list of all netcdf files in folder (one for each year)
all_files <- dir("~/Workspace/RioSaoFrancisco/Data/NCEP/Test", full.names = TRUE)
all_files_short <- dir("~/Workspace/RioSaoFrancisco/Data/NCEP/Test")

## check for CRS of these NetCDF
## check extend of the ERA5 files to subset from WatchERA data
test <- nc_open(all_files[1])

test2 <- nc_open("Data/WatchERA5 - RAW/Rainf_WFDE5_CRU+GPCC_197902_v1.0.nc")
 
print(test2)


## stack all the netcdf files into one raster stack. One raster per timestep (day)
ncdf_stack <- stack(all_files)

# load shapefile of subbasins for extraction
subbasins <- readOGR("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/GIS/75subbas-neu/Shape/75SubbasWGS84.shp")

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

#Save workspace to save time for future runs
#save.image(file = "workspace_NetCDF.RData")

colnames(DF_mean) <- subbasins@data$DN

## create date vector for dataframe
start <- as.POSIXct("01.01.2000", format = c("%d.%m.%Y"), tz = "UTC")
end <- as.POSIXct("31.12.2009", format = c("%d.%m.%Y"), tz = "UTC")
time_vector <- seq(start,end, by ="day")
DF_mean$Date <- time_vector

write.csv(DF_mean, file = "ERA52000_2009_T2m_at_75subbasins.txt", row.names = FALSE)






rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(ncdf4)
#library(RNetCDF)
library(raster)
#library(ncdf.tools)
library(PaulsPack)

## create a list of all netcdf files in folder (one for each year)
all_files <- as.list(dir("~/Workspace/RioSaoFrancisco/Data/NCEP/relative_humidity", full.names = TRUE))



install_github("jmigueldelgado/hymet-datasets")
## extent of RSF basin
##xmin -49, ymin -22.5, xmax -35, ymax -2.5
#Das sind die offiziellen SaWaM-Randkoordinate
coor <- data.frame(lon=13.40,lat=52.52)
var <- c('temperature','relative humidity')
years <- c('2000','2001')

request <- def_request(coor,var,years)

get_nc(request)

nc2rds(request)
install_github("plvoit/PaulsPack")
library(PaulsPack)

#https://dominicroye.github.io/en/2018/access-to-climate-reanalysis-data-from-r/
install.packages("RNCEP")
