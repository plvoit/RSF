rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

## Mean Prec from Gerds Station (ID 300)
P <- read.csv("Data/xds_gerd/xds_gerd/brazil/obs/P.csv", check.names = F)

P <- P[,c(1,2,3,54)]
P <- P[9132:nrow(P),]
P[P == -999] <-  NA

P$Date <- paste(P$Y,P$M,P$D, sep = "-")
P$Date <- as.POSIXct(P$Date, format = c("%Y-%m-%d"))
plot(P[,4]~P$Date, type = "l")

five_years <- P[P$Date > "2012-01-01", ] 
plot(five_years[,4]~five_years$Date, type = "l")

#aggregate to yearly Prec
P_yearly <- aggregate_by_time(P,4,"1 year", sum)
names(P_yearly)[1] <- 'Date' 
P_yearly$Date  <- as.POSIXct(P_yearly$Date)

mean(P_yearly[,2])

# Mean Prec from ERA5
rain_daily <- read.csv("Data/Complete Timeseries Input 1981-2019/ERA5-Land-1981-2019/WASA_Time_series_ERA5Land_1981-2019/rain_daily.dat", header=T, skip = 2, sep = "\t", check.names = F)
rain_daily <- rain_daily[,c(1,77)]

rain_daily$year <- substr(rain_daily[,1],nchar(rain_daily[,1])-3,nchar(rain_daily[,1]))
rain_daily$year <-  as.factor(rain_daily$year)
#rain_daily$month <- substr(rain_daily[,1],nchar(rain_daily[,1])-5,nchar(rain_daily[,1])-4)

era5_yearly <- aggregate(rain_daily[,2],by = list(rain_daily$year), sum)
names(era5_yearly)[1] <-  'year'

plot(P_yearly[,2], type = "l" )
lines(era5_yearly[,2], type = "l", col = "red")

mean(P_yearly[,2])
mean(era5_yearly[,2])

### Split observed prec into rainy and dry season
## x% of the preciptition fall in during the rainy season

P$season <- 0

for (i in 1:nrow(P)){
  if (P[i,2] %in% c(11,12,1,2,3)) P[i,6] <- "rainy"
  if (P[i,2]%in% c(4,5,6,7,8,9,10)) P[i,6] <- "dry"
}

seasonal_P <- aggregate(P[,4],by = list(P[,6]), sum, na.rm = T)
