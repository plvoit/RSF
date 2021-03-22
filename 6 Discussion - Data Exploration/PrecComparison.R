##This script is for comparing the subbasin precipitation input from ERA5 (the mean of the cells within the subbasin)
## to rain gauge stations close by. A list was made that contains subbasins and their close by rain gauges ("SubbasinandClosePrecStations.txt")

# (C) 2021 Paul Voit

rm(list = ls())

setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

Pobs <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/obs/P.csv", check.names = F, na.strings = "-999.0" )
Pobs$Date <- paste(Pobs$Y,Pobs$M,Pobs$D, sep = "-")
Pobs$Date <- as.POSIXct(Pobs$Date, format = c("%Y-%m-%d"))
Pobs <- Pobs[,c(155,1:154)]


ERA5 <- read.csv("~/Workspace/RioSaoFrancisco/Data/InputWASA2000-2009 timeseries/rain_daily.dat", skip = 2, sep = "\t", check.names = F, colClasses = c("character", rep("numeric",77)))
ERA5$Date <- as.POSIXct(ERA5$`0`, format = c("%d%m%Y"))
ERA5 <- ERA5[,c(79,3:77)]

Pobs <- Pobs[Pobs$Date <= max(ERA5$Date),]
Pobs <- Pobs[Pobs$Date >= min(ERA5$Date),]

# Subbasin and close by rain gauges
Sub_station <- read.delim("~/Workspace/RioSaoFrancisco/Data/PrecComparison/SubbasinandClosePrecStations.txt")

ERA5_month <- aggregate_by_time(ERA5,c(2:76),"month",sum)
names(ERA5_month)[1] <- "Date"
Pobs_month <- aggregate_by_time(Pobs,c(2:155),"month",sum)
names(Pobs_month)[1] <- "Date"


for (i in 1:nrow(Sub_station)){
  plot(ERA5_month[,match(Sub_station[i,1],colnames(ERA5_month))]~ERA5_month$Date, main = paste("Sub",Sub_station[i,1]), type = "l", ylab = "Precipiation [mm]")
  lines(Pobs_month[,match(Sub_station[i,2],colnames(Pobs_month))] ~ Pobs_month$Date, col = "red")
  legend("topright", legend = c("observed", "ERA5"), col = c("black", "red"), lt = 1)
  }

## Residuals
res_vec <- c()

for (i in 1:nrow(Sub_station)){
  res <- ERA5_month[,match(Sub_station[i,1],colnames(ERA5_month))] - Pobs_month[,match(Sub_station[i,2],colnames(Pobs_month))]
  plot(res ~ ERA5_month$Date, type = "l", main=paste("Residuals (ERA5-obs) Sub",Sub_station[i,1]), ylab = "Residual [mm]")
  abline(h= 0)
  res_vec[i] <- sum(res)
}

names(res_vec) <- Sub_station[,1]
res_vec
