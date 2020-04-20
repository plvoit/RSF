### this script was intended to find a set of min. 20 stations that have no to many NA's over the longest possible period
### to be used for a PCA analysis. I wonder if there's a package for this.
### F.e. something like this: criteria: min stations, min time span, max. accepted NA-period within (f.e. 2 days)
### Result: I couldn't find any timeseries matching these criteria 

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

Pera <- read.csv("~/Workspace/RioSaoFrancisco/Data/ERA51981_2019_precipitation_at_stations.txt", check.names = FALSE)
Pobs <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/obs/P.csv", na.strings = c("-999","-999.0","-99.9"), check.names = FALSE)


## format Date
Pera$Date <- as.POSIXct(Pera$Date)


Pobs$Date <- as.POSIXct(paste(Pobs$D,Pobs$M,Pobs$Y, sep= "."),tz ="GMT",format = c("%d.%m.%Y"))
Pobs <-  Pobs[,c(ncol(Pobs),1:ncol(Pobs)-1)]
Pobs <-  Pobs[,c(1,5:ncol(Pobs))]

### filter out the stations that are not included in obs
Pobs <- Pobs[,colnames(Pera)]

### select the dates contained in both dataframes
Pobs <- Pobs[Pobs$Date >= min(Pera$Date)  ,]
Pera <- Pera[Pera$Date <= max(Pobs$Date),]


Pobs$Year <- format(Pobs$Date, format = "%Y")
years <- unique(Pobs$Year)

na_yearly <- matrix(nrow = length(years), ncol = 65)
na_yearly[,1] <- as.numeric(years)
na_yearly <- as.data.frame(na_yearly)
names(na_yearly)[2:ncol(na_yearly)] <- colnames(Pobs)[2:(ncol(Pobs)-1)]
names(na_yearly)[1] <- "Year"


for (i in 1:nrow(na_yearly)){
  na_yearly[i,2:ncol(na_yearly)] <- sapply(Pobs[Pobs$Year == years[i],2:ncol(na_yearly)], function(y) sum(length(which(is.na(y)))))
}


na_yearly$SumNA <- rowSums(na_yearly[,2:ncol(na_yearly)])

# kick out columns by hand (ID 130,116,158,193,198,201,202,207,209,221,228,229,242,286,288,289,317,323,335,342,388,290)
outcols <- as.character(c(130,116,158,193,198,201,202,207,209,221,228,229,242,286,288,289,317,323,335,342,388,290))
na_yearly[,outcols] <- list(NULL)


## remove years 1981-1990
na_yearly <- na_yearly[14:nrow(na_yearly),]

##remove years 2016-2018
na_yearly <- na_yearly[1:(nrow(na_yearly)-3),]

## Na sums by station
station_na <- apply(na_yearly,2,sum)
#obs_complete <- Pobs[complete.cases(Pobs),]

na_yearly <- rbind(na_yearly,station_na)

summary(Pobs[1:365,])

na_count <-sapply(Pobs[,2:ncol(Pobs)], function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
