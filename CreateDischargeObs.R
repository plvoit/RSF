# the discharge_obs_24.txt is a file to compare the results of WASA with the meassured values at the
# gauges.
# This script uses the resulting files (dataframes) of the scripts Q_ANAbyLuis and Q_Funceme where
# all the runoff data was processed and put into one dataframe.The timeseries from the reservoir data by ONS
# is also added. This files get loaded now and
# the stations which will be used in the model will be selected.
# The corresponding subbasin ID from lumpR for each gauge is found and then used as columnname instead
# As last step the data will be formated in a way as it is expected by WASA
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

Q_ANA <- read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/Q_ANA_corrected.txt")
Q_ANA$date <- as.POSIXct(Q_ANA$date)
names(Q_ANA)[1] <- "Date"

Q_FUNCEME <- read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/Q_FUNCEME_corrected.txt")
Q_FUNCEME$Date <- as.POSIXct(Q_FUNCEME$Date)
# create a dataframe of ONS Vazao Natural (naturalized run off)
# for Sobradinho, Tres Marias and Xingo

ONS_files <- dir("Data/ONS reservoir data/Workdir", full.names = TRUE)
ONS_files_short <- dir("Data/ONS reservoir data/Workdir", full.names = FALSE)

ONS_list <- lapply(ONS_files, function(x) read.csv2(x))
ONS_list <- lapply(ONS_list, function(x) x <- x[,c(2,8,11)])
ONS_list <- lapply(ONS_list, function(x) {names(x)[3] <- "Date"; x})
ONS_list <- lapply(ONS_list, function(x) {x[,3] <- as.POSIXct(x[,3], format = c("%d.%m.%Y")); x})
ONS_list <- lapply(ONS_list, function(x) {x[,1] <- as.character(x[,1]); x})

# name the columns with the names of the reservoirs
name_vector <-  c()
for (i in 1:length(ONS_list)){
  name_vector[i] <- ONS_list[[i]][1,1]
}

for (i in 1:length(ONS_list)){
  names(ONS_list[[i]])[2] <- name_vector[i]
}

#now get rid of the name colum
ONS_list <- lapply(ONS_list, function(x) {x <- x[,c(2,3)] })

# merge the list into one dataframe
ONS_DF <- Reduce(function(x,y) merge(x,y, by = "Date", all.x = TRUE, all.y = TRUE), ONS_list)

# select the stations used for the model

ANA_stations_used <-  c("A_42980000")
FUNCEME_stations_used <- c("F_40050000","F_40100000","F_42210000","F_45298000","F_46035000","F_46360000","F_48590000",
                      "F_49705000","F_40850000","F_41990000","F_43880000","F_45260000","F_46902000")

Q_ANA <- Q_ANA[,c("Date",ANA_stations_used)]
Q_FUNCEME <- Q_FUNCEME[,c("Date",FUNCEME_stations_used)]

#merge ONS,ANA and FUNCEME
dummy <- merge(ONS_DF,Q_ANA, by = "Date", all.x = TRUE, all.y = TRUE)
stations_model <- merge(dummy,Q_FUNCEME, by = "Date", all.x = TRUE, all.y = TRUE )
# fix the weird brazilian character
names(stations_model)[3] <- "TRES MARIAS"

# fill gaps in date column
stations_model <- fillNA_datesequence(stations_model,"1 day")

### RENAME RESERVOIR COLUMNS

# get the right subbasin number for every gauge. This happens by hand, looking in QGis.

Gauges <- c("F_49705000", "XINGO","F_48590000","SOBRADINHO", "F_46360000", "F_46902000", "F_46035000",
           "F_45298000","F_45260000","F_43880000", "F_42210000","A_42980000", "F_41990000","TRES MARIAS",
           "F_40100000","F_40850000", "F_40050000")
Subbas_ID <- c("1","2","3","4","96","45", "58","90","16","15","78","73","13","9","10","12","11")

#check if no mistake was made
length(Gauges[Gauges %in% colnames(stations_model)])

# create dataframe giving each Gauge the corresponding subbasin ID
SubbasID_GaugeNumber <- data.frame(Gauges,Subbas_ID)
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)
SubbasID_GaugeNumber$Subbas_ID <- as.character(SubbasID_GaugeNumber$Subbas_ID)

# renaming the columns with subbasin ID rather than gauge number, for this the function match() works well
for (i in 2:18){
  names(stations_model)[i] <- SubbasID_GaugeNumber[match(names(stations_model[i]), SubbasID_GaugeNumber$Gauges),"Subbas_ID"]
  
}

# the dataframe needs to be in a certain format to run the test on WASA

stations_model$YYYY <- format(stations_model$DateTime, "%Y")
stations_model$MM <- format(stations_model$DateTime, "%m")
stations_model$DD <- format(stations_model$DateTime, "%d")
stations_model$HH <- 0

# take the dates from 2000-2009
stations_model <- stations_model[stations_model$DateTime >= "2000-01-01" & stations_model$DateTime <= "2009-12-31", ]

#reorder and kick out DateTime column
stations_model <- stations_model[,c(19,20,21,22,2:18)]
stations_model[,4:21] <- round(stations_model[,4:21],2)

#save file
write.table(stations_model,file ="discharge_obs_24.txt", sep = "\t", row.names = FALSE, quote = FALSE)

#the necessary header will be attached manually in the text editor

