# the results for Urucuia subbasin are not good with ERA5 precipitation. station data from Gerd is available. 
# this script creates a new rain_daily.dat using this data for precipitation
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
P <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/obs/P.csv", check.names = F)

#P$Date <- paste(P$Y,P$M,P$D)

P <- P[P$Y > 1999 & P$Y < 2010,c(1,2,3,54) ]
P[P == -999] <- NA

plot(P[,4], type = "l")
lines(ERA[,2], type = "l", col = "red")
ERA <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/BaseLineNewIrriData/init_config/Input/Time_series/rain_daily.dat", skip = 2, header = T, sep = "\t", check.names = F)
ERA <- ERA[,c(1,77)]


## Fill gaps in observed P with ERA5-Data

for (i in 1:nrow(P)){
  if(is.na(P[i,4])) P[i,4] <- ERA[i,2]
}

Pnew <- P

### add this to rain_daily.dat file in the timeseries folder
### Header has to be added 


ERA <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/BaseLineNewIrriData/init_config/Input/Time_series/rain_daily.dat", skip = 2, header = T, sep = "\t", check.names = F)

ERA[,77] <- Pnew[,4]

f <- file("rain_daily.dat", "w")
writeLines("Daily total precipitation [mm] for each subasin, ordered according to Map-IDs",f)
writeLines("Date		Subbasin-ID.",f)
write.table(ERA,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)

#write.table(ERA,file = "rain_daily.dat", sep = "\t", row.names = FALSE, quote = FALSE)

ERA$Year <- as.numeric(substr(ERA[,1],nchar(ERA[,1])-3,nchar(ERA[,1])))
ERAyearly <- aggregate(ERA,list(ERA$Year),sum)

P <- P[P$Y > 1999 & P$Y < 2010,c(1,2,3,54) ]
P[P == -999] <- NA
Pyearly <- aggregate(P, list(P$Y), sum, na.rm = T)

Pyearly


plot(ERAyearly[,77], type = "l", col = "red", ylim = c(500,1800) )
lines(Pyearly[,5], type = "l")

sum(ERA[,2])

sum(P[,4], na.rm = T)

years <- seq(2000,2009,1)

for (i in 1:length(years)){
  dummyP <- P[P$Y == years[i],]
  dummyERA <- ERA[ERA$Year == years[i],]
  plot(dummyERA[,2], type = "l", col = "red")
  lines(dummyP[,4], type = "l")
}
