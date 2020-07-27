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

# create new column with clearer names
# import attribute table from QGIS
attr_table <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/stations_model_CSV.csv", stringsAsFactors = FALSE)
attr_table[1,1] <- "F_49705000"

SubbasID_GaugeNumber <- merge(SubbasID_GaugeNumber, attr_table, by.x = "Gauges", by.y = "ID")
SubbasID_GaugeNumber <- SubbasID_GaugeNumber[,c(1,2,6)]


for (i in colnames(obs)[5:21]){
  plot(obs[[i]]~obs$Date, type = "l", ylim = c(0,7000), xlab = "Date", ylab  = "m^3/s",
       main = SubbasID_GaugeNumber[match(i, SubbasID_GaugeNumber$Subbas_ID),"km.source"])
  lines(WASA[[i]]~WASA$Date, type = "l", col = "red")
  legend("topright", legend = c("Observed","WASA pred"), col = c("black","red"), lty=1, cex=0.8)
}

## code von Daniel Niederschlag von oben
barplot(Precip_HH$PrecipSums_mm, ylim = rev(c(0,25)), 
yaxt = "n", col = rep( "dodgerblue", 
length(Precip_HH$PrecipSums_mm)), border = "dodgerblue",
ylab = "Precipitation [mm]", cex.lab = 0.8)axis(2, at = seq(0, 25, by = 5), cex.axis = 0.8)
box(bty = "7")
