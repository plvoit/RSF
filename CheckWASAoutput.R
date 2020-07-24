rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")


WASA <- read.table("~/Workspace/RioSaoFrancisco/WASA-SED/WASA-SED/0/Output/River_Flow.out", quote="\"", comment.char="", 
                   skip = 1, header = TRUE, check.names = FALSE )

WASA$Date <-  seq(as.POSIXct("2000-01-01"), as.POSIXct("2009-12-31"), "days")

obs <- read.delim("~/Workspace/RioSaoFrancisco/Data/InputWASA2000-2009 timeseries/discharge_obs_24.txt",
                               header=TRUE, skip = 4, check.names = FALSE)

obs$Date <- seq(as.POSIXct("2000-01-01"), as.POSIXct("2009-12-31"), "days")

obs <- obs[obs$Date <= "2009-12-31",]

# load GaugeNumber-SubbasID file
SubbasID_GaugeNumber <- read.delim("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)


for (i in colnames(obs)[5:21]){
  plot(obs[[i]]~obs$Date, type = "l", ylim = c(0,7000), xlab = "Date", ylab  = "m^3/s",
       main = SubbasID_GaugeNumber[match(i, SubbasID_GaugeNumber$Subbas_ID),"Gauges"])
  lines(WASA[[i]]~WASA$Date, type = "l", col = "red")
  legend("topright", legend = c("Observed","WASA pred"), col = c("black","red"), lty=1, cex=0.8)
}

