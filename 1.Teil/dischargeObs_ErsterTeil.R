rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

gauges <- c("F_40050000","F_40100000")

## mit TRES Marias Afluencia?

stations_model <- read.csv("Data/Runoff-data/Q_FUNCEME.txt", stringsAsFactors = FALSE)

stations_model <-  stations_model[,c("Date",gauges)]

# rename gauges with Subbasin IDs from WASA
SubbasID <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")

for (i in 2:ncol(stations_model)){
  names(stations_model)[i] <- SubbasID[match(names(stations_model)[i], SubbasID$Gauges),"Subbas_ID"]
}

stations_model$Date <- as.POSIXct(stations_model$Date, format = c("%Y-%m-%d"))

stations_model$YYYY <- format(stations_model$Date, "%Y")
stations_model$MM <- format(stations_model$Date, "%m")
stations_model$DD <- format(stations_model$Date, "%d")
stations_model$HH <- 0

# take the dates from 2000-2009
stations_model <- stations_model[stations_model$Date >= "2000-01-01" & stations_model$Date <= "2009-12-31", ]


#reorder and kick out DateTime column
stations_model <- stations_model[,c(4,5,6,7,2,3)]
stations_model[,5:6] <- round(stations_model[,5:6],1)

#save file
write.table(stations_model,file ="discharge_obs_24.txt", sep = "\t", row.names = FALSE, quote = FALSE)




