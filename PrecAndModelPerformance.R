rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

### Pobs = Observed precip., Pera: predicted by ERA5
Pera <- read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/ERA51981_2019_precipitation_at_stations.txt", check.names = FALSE)
Pobs <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/obs/P.csv", na.strings = c("-999","-999.0","-99.9"), check.names = FALSE)

Pera$Date <- as.POSIXct(Pera$Date, format = c("%Y-%m-%d"))

Pobs$Date <-as.POSIXct(paste(Pobs$Y,Pobs$M,Pobs$D, sep = "-"), format = c("%Y-%m-%d"))

# select calibration period 2000-2009
Pobs <-  Pobs[Pobs$Date >= "2000-01-01" & Pobs$Date < "2010-01-01",]
Pera <-  Pera[Pera$Date >= "2000-01-01" & Pera$Date < "2010-01-01",]

# Subbasin and close by rain gauges
Sub_station <- read.delim("~/Workspace/RioSaoFrancisco/Data/PrecComparison/SubbasinandClosePrecStations.txt")

#Select stations from Sub_stations, keep date_vector

Pera <- Pera[,c(1,match(Sub_station$Station,colnames(Pera)))]
Pobs <- Pobs[,c(1,match(Sub_station$Station,colnames(Pobs)))]
Pobs <- Pobs[,-1]
Pobs$Date <- Pera$Date
Pobs <- Pobs[,c(27,1:26)]

test <- apply(Pera[,2:ncol(Pera)],2,sum)
test2 <-apply(Pobs[,2:ncol(Pobs)],2,sum, na.rm = TRUE)

# residual of the precipitation sum over the 10 years
res_sum <- test2-test
res_sum
summary(res_sum)

res_sum <- sqrt((test - test2)^2)
res_sum <- as.data.frame(res_sum)
res_sum$Sub <- Sub_station$Sub


### select subs that have discharge obs

test <- c(4,5,6,7,8,11,12,13,15,16,22,26)
res_sum <-  res_sum[test,]
res_sum <-  res_sum[-10,]
res_sum$modelPerformance <- c(-0.11,-0.11,-0.11,0.48,0.48,-0.6,0.195,-0.43,-0.61,0.87,0.59)

res_sum <- res_sum[-3,]

plot(res_sum$res_sum ~ res_sum$modelPerformance, ylab = expression("e"[p]*" mm"),xlab = "Model performance NSE", main = expression("e"^p*" ~ NSE" ))
linmod <- lm(res_sum$res_sum ~ res_sum$modelPerformance)
abline(1783.9,-304.8, lty = "dashed", col = "red")
mtext("y = 1783.9 - 304.8 * x \n p = 0.82", line = -5)

summary(linmod)

for (i in 2:ncol(Pera)){
plot(Pera[,i]~Pera$Date)
points(Pobs[,i]~Pobs$Date, col = "red")
}

# Plot residuals
res <- Pobs[,2:ncol(Pobs)] - Pera[,2:ncol(Pera)]
res$Date <- Pobs$Date

for ( i in 1:(ncol(res)-1)) {
  plot(res[,i]~res$Date, type = "l", main = expression("P"["obs"] *" -P"["ERA5"]), ylab = "residuals [mm]", xlab = "year")
}

## Examples for expression()
#plot(1:10, xlab=expression('hi'[5]*'there'[6]^8*'you'[2]))
#plot(1,1, main=expression('title'^2))  #superscript
#plot(1,1, main=expression('title'[2])) #subscript

summary(res)
#res <- sqrt((res)^2)
#RMSE <- apply(res,2,mean, na.rm = T)

#RMSE <- as.data.frame(RMSE)
#RMSE$Sub <- Sub_station$Sub
#names(RMSE)[1] <- "RMSE [mm]"
