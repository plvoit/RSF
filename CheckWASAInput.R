# This script formats the timeseries outputs from getNCEP and ExtractSubbasinMeans and formats them in the right way
# for WASA input:
# rain, humidity, radiation, temperature

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

Precipitation <- read.csv("~/Workspace/RioSaoFrancisco/Backupcopy Input WASA/ERA52000_2009_precipitation_at_75subbasins.txt", check.names = FALSE)
Precipitation[,1:75] <- round(Precipitation[,1:75],1)
SSRD <- read.csv("~/Workspace/RioSaoFrancisco/Backupcopy Input WASA/ERA52000_2009_SSRD_at_75subbasins.txt", check.names = FALSE)
SSRD[,1:75] <- round(SSRD[,1:75],1)
T2m <- read.csv("~/Workspace/RioSaoFrancisco/Backupcopy Input WASA/ERA52000_2009_T2m_at_75subbasins.txt", check.names = FALSE)
T2m[,1:75] <- round(T2m[,1:75],1)
rHum <- read.csv("~/Workspace/RioSaoFrancisco/Backupcopy Input WASA/NCEP2000_2009_rHum_at_75subbasins.txt", check.names = FALSE)
rHum[,2:76] <- round(rHum[,2:76],0)

input_list <-  list(Precipitation,SSRD,T2m,rHum)

# make vector with the order of the list for later naming the individual files

naming_vector <- c("rain_daily", "radiation", "temperature", "humidity")

# change the order of columns and format the date
input_list <- lapply(input_list, function(x) x <- x[,c("Date",names(x))])
input_list <- lapply(input_list, function(x) { x["Date.1"] <- NULL; x })  # ,x within function seems to be the return value, otherwise you'd get nothing
input_list <- lapply(input_list, function(x) {x [,"Date"] <- as.POSIXct(x[,"Date"], format = c("%Y-%m-%d")); x} )

## change the format of the date as it's wanted for WASA
input_list <- lapply(input_list, function(x) { x[,"Date"] <- format(x[,"Date"], "%d%m%Y") ;x } )

# add number of days column, the timeseries are in daily resolution
input_list <- lapply(input_list, function(x) {x["No. of days"] <- 1:nrow(x) ; x })

# change the order of the columns
input_list <- lapply(input_list, function(x) x <- x[,c("Date","No. of days", names(x))])
input_list <- lapply(input_list, function(x) { x[c("Date.1","No. of days.1")] <- NULL; x })

# the WASA input files have a certain header structure. This is way the column name for Date and No. of days will
# will be changed to zero

input_list <- lapply(input_list, function(x) {names(x)[c(1,2)] <- c("0","0"); x })

# save all the files in a folder. The header will be addded manually since it's just four timeseries files

for (i in 1:length(input_list)){
  write.table(input_list[[i]],file = paste(naming_vector[i],".dat", sep = ""), sep = "\t", row.names = FALSE, quote = FALSE)

}
