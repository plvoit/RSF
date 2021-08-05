rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

obs <- read.delim("~/Workspace/RioSaoFrancisco/Data/Complete Timeseries Input 1981-2019/WASA_Time_series_Discharge_1981-2019/discharge_obs_24.txt",skip = 4, header=T, check.names = F)

obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))

obs <- obs[obs$Date >= '2000-01-01',]

for (i in 5:(ncol(obs)-1) ){
  plot(obs[,i]~obs$Date, type = "l", main = names(obs)[i] )
  
}
