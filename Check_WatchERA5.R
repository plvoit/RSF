### This script does an analysis and visualisation of predicted precipitation values compared to
### observed values. Monthly, and yearly sums, correlation, error analysis, sums of squares etc...
### WatchERA5 is a bias corrected hindcast from 1979-2016

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

Pera <- read.csv("~/Workspace/RioSaoFrancisco/Data/WatchERA51979_2016_precipitation_at_stations.txt", check.names = FALSE)
names(Pera)[1] <- "Date"

## WatchERA5 gives precipitation in kg/m^2s, this has to be changed to mm. 1 mm == 1kg H2O per m^2

Pera[,2:ncol(Pera)] <- Pera[,2:ncol(Pera)]*3600*24


Pobs <- read.csv("~/Workspace/RioSaoFrancisco/Data/xds_gerd/xds_gerd/brazil/obs/P.csv", na.strings = c("-999","-999.0","-99.9"), check.names = FALSE)


## format Date
Pera$Date <- as.POSIXct(Pera$Date)

Pobs$Date <- as.POSIXct(paste(Pobs$D,Pobs$M,Pobs$Y, sep= "."),tz ="GMT",format = c("%d.%m.%Y"))
Pobs <-  Pobs[,c(ncol(Pobs),1:ncol(Pobs)-1)]
Pobs <-  Pobs[,c(1,5:ncol(Pobs))]

### filter out the stations that are not included in obs
Pobs <- Pobs[,colnames(Pera)]

### select the dates contained in both dataframes
Pobs <- Pobs[Pobs$Date >= min(Pera$Date),]
Pobs <- Pobs[Pobs$Date <= max(Pera$Date), ]
Pera <- Pera[-nrow(Pera),]


#plot all the stations observerd and measured daily values
#for ( i in 2:ncol(Pobs)){
#  plot(Pobs[,i]~Pobs$Date, type = "l", ylab= "P", main =paste(colnames(Pobs)[i],"Daily Precipitation", sep = " "))
#  lines(Pera[,i]~Pera$Date, type = "l", col = "red")
# legend("topright", legend = c("Observed","Hindcast"), col = c("black","red"), lty=1, cex=0.8)
# }


## create monthly sums
Pobs_month <- aggregate_by_time(Pobs,c(2:ncol(Pobs)),"1 month", sum, right = FALSE)
Pera_month <- aggregate_by_time(Pera,c(2:ncol(Pera)),"1 month", sum, right = FALSE)

Pobs_month$Date <- as.POSIXct(Pobs_month$Group.1,tz ="GMT",format = c("%Y-%m-%d"))
Pera_month$Date <- as.POSIXct(Pera_month$Group.1,tz ="GMT",format = c("%Y-%m-%d"))

Pera_month <- Pera_month[,c(ncol(Pera_month),3:ncol(Pera_month)-1)]
Pobs_month <- Pobs_month[,c(ncol(Pobs_month),3:ncol(Pobs_month)-1)]

## Aggregation changes NA to 0. Change back. ## It's not clear if there are month with 0 precipitation or not
#Pobs_month[Pobs_month == 0] <-  NA 

### plot all the stations observerd and measured monthly values
#for ( i in 2:ncol(Pobs_month)){
# plot(Pobs_month[,i]~Pobs_month$Date, type = "l", ylab= "P",main =paste("Monthly Precipitation",names(Pobs_month[i])))
# lines(Pera_month[,i]~Pera_month$Date, type = "l", col = "red")
# legend("topright", legend = c("Observed","Hindcast"), col = c("black","red"), lty=1, cex=0.8)
#}


### Create the  monthly residuals for every station  ## apparently to calculate transformation to matrix is not needed
Pres_month <- Pera_month[,2:ncol(Pera_month)] - Pobs_month[,2:ncol(Pobs_month)]
Pres_month <- cbind(Pobs_month$Date, Pres_month)
names(Pres_month) <- c("Date",names(Pobs_month)[2:ncol(Pobs_month)])

## plot all the stations observerd and measured monthly residuals (ERA5 - Observed)
#for ( i in 2:ncol(Pobs_month)){
 # plot(Pres_month[,i]~Pres_month$Date, type = "l", ylab= "P",main =paste("Monthly Residuals Precipitation",names(Pobs_month[i])))
#}

###Yearly Sums
Pobs_year <- aggregate_by_time(Pobs,c(2:ncol(Pobs)),"1 year", sum, right = FALSE)
Pera_year <- aggregate_by_time(Pera,c(2:ncol(Pera)),"1 year", sum, right = FALSE)

Pobs_year$Date <- as.POSIXct(Pobs_year$Group.1,tz ="GMT",format = c("%Y-%m-%d"))
Pera_year$Date <- as.POSIXct(Pera_year$Group.1,tz ="GMT",format = c("%Y-%m-%d"))

Pera_year <- Pera_year[,c(ncol(Pera_year),3:ncol(Pera_year)-1)]
Pobs_year <- Pobs_year[,c(ncol(Pobs_year),3:ncol(Pobs_year)-1)]

### To aggregate to yearly sums, years with only NA appear as sum = 0. Set back years with 0 to NA
### that in a whole year no precipitation occured
Pobs_year[Pobs_year == 0 ] <- NA

### now with this the yearly residuals have to be calculated again
Pres_year <- (Pera_year[,2:ncol(Pera_year)]-Pobs_year[,2:ncol(Pobs_year)])
Pres_year <- cbind(Pobs_year$Date, Pres_year)
names(Pres_year) <- c("Date",names(Pres_year)[2:ncol(Pres_year)])


### yearly RMSE per station
RMSE_yearly <-  c()

for ( i in 2:ncol(Pres_year)){
 RMSE_yearly[i] <- mean(sqrt(Pres_year[,i])^2, na.rm = TRUE)
 names(RMSE_yearly)[i] <- names(Pres_year)[i]
}

RMSE_yearly

### mean yearly Precipitation observed
mean_obs <- c()
for ( i in 2:ncol(Pobs_year)){
  mean_obs[i] <- mean(Pobs_year[,i], na.rm = TRUE)
  names(mean_obs)[i] <- names(Pobs_year)[i]
}

mean_obs

### mean error in percent
mean_error_percent <- round(RMSE_yearly / mean_obs * 100, digits = 1)
mean_error_percent
summary(mean_error_percent)

length(mean_error_percent[mean_error_percent <= 25])

# correlatiocoeff. of daily values
correlation <- c()
for (i in 2:ncol(Pobs) ){
  correlation[i] <- cor(Pobs[,i],Pera[,i],use = "complete.obs")
  names(correlation)[i] <- names(Pobs)[i]
}
summary(correlation)


## aggregate residuals for each month -> which month have the highest uncertainty?
Pres_month$month <- as.factor(format(Pres_month$Date, format = c("%m")))
Monthly_res <- aggregate(Pres_month[,2:ncol(Pres_month)-1], by = list(Pres_month$month),mean,na.action = na.omit)
Monthly_res <-  Monthly_res[,c(1,3:ncol(Monthly_res))]
names(Monthly_res)[1] <-  "Month"

# plot the mean monthly residuals for each station
for (i in 2:ncol(Monthly_res)){
plot(Monthly_res[,i]~Monthly_res$Month, xlab = "Month", ylab = "Residual (mm)", main = colnames(Monthly_res)[i])
}

#### mean monthly error for all stations
Mme_total <- rowMeans(Monthly_res[,2:ncol(Monthly_res)])
Mme_total <- as.data.frame(cbind(Monthly_res$Month,Mme_total))
names(Mme_total)[1] <- "Month"
Mme_total

### mean monthly observed precipitation
Pobs_month$month <- as.factor(format(Pobs_month$Date, format = c("%m")))
Monthly_obs <- aggregate(Pobs_month[,2:ncol(Pobs_month)-1], by = list(Pobs_month$month),mean,na.action = na.omit)
Monthly_obs <-  Monthly_obs[,c(1,3:ncol(Monthly_obs))]
names(Monthly_obs)[1] <-  "Month"

### mean monthly predicted precipitation
Pera_month$month <- as.factor(format(Pera_month$Date, format = c("%m")))
Monthly_era <- aggregate(Pera_month[,2:ncol(Pera_month)-1], by = list(Pera_month$month),mean,na.action = na.omit)
Monthly_era <-  Monthly_era[,c(1,3:ncol(Monthly_era))]
names(Monthly_era)[1] <-  "Month"



## mean (over all stations) monthly observed precipitation
Mmo_total <- rowMeans(Monthly_obs[,2:ncol(Monthly_obs)])
Mmo_total <- as.data.frame(cbind(Monthly_obs$Month, Mmo_total))
names(Mmo_total)[1] <- "Month"
Mmo_total

## mean (over all stations) monthly predicted precipitation
Mmp_total <- rowMeans(Monthly_era[,2:ncol(Monthly_era)])
Mmp_total <- as.data.frame(cbind(Monthly_era$Month, Mmp_total))
names(Mmp_total)[1] <- "Month"
Mmp_total

plot(Mmp_total$Mmp_total~as.numeric(Mmo_total$Month), type = "l", col="red", ylim = c(0,200), xlab = "Month", ylab = "P [mm]", main = "Mean monthly precipitation")
lines(Mmo_total$Mmo_total~as.numeric(Mmo_total$Month), type ="l")
legend("topright", legend = c("Observed","Predicted"), col = c("black","red"), lty=1, cex=0.8)


## how well fit both curves together. Sum of squares
SumSq <- sum(Mmo_total[2] - Mmp_total[2])^2

### Die Vorhersage scheint der Regenzeit zu überschätzen.

write.csv(Mmp_total,"Predicted_WatchERA5.txt", row.names = FALSE)


### mean (over all stations) monthly error in percent of the mean observed precipitation per month
Mme_percent <-  Mme_total$Mme_total/Mmo_total$Mmo_total * 100
names(Mme_percent) <- Mme_total$Month
Mme_percent
summary(Mme_percent)
plot(Mme_percent~Mmo_total$Month, type = "l", xlab = "Month", ylab ="[%]",main = "Mean error [%]")

### Der durchschnittliche prozentuale Fehler ist am höchsten in der Trockenzeit
#for ( i in 2:ncol(Monthly_res)){
#  barplot(Monthly_res[,i]~Monthly_res$Month, main = paste("Residuals", colnames(Monthly_res)[i]), xlab = "Month")
#}
# evt. crosscorrelation
# autocorrelation of residual plot ?
## prozentualer Residuenplot?


## Station information
stations <- read.csv("~/Workspace/RioSaoFrancisco/xds_gerd/xds_gerd/brazil/meta.txt", comment.char="#")

### plot mean_yearly_residuals against station altitude

mean_error_percent <- as.data.frame(mean_error_percent)
mean_error_percent <- cbind(names(Pobs)[2:length(names(Pobs))], mean_error_percent[2:nrow(mean_error_percent),])
names(mean_error_percent) <-  c("id","mean error %")


stations_merge <- merge(stations, mean_error_percent, by.x = "id", by.y = "id")
stations_merge <- stations_merge[stations_merge$varname == "precipitation",]
stations_merge$`mean error %` <- as.numeric(stations_merge$`mean error %`)

plot( stations_merge$`mean error %` ~ stations_merge$alt )
plot(stations_merge$`mean error %`~stations_merge$lat)
plot(stations_merge$`mean error %`~stations_merge$lon)

#### The residuals don't seem to be related to the altitude, lat or lon of the station



### Save interactive plots

### Save all the files as interactive dygraph plot (Raw_Data)
library(dygraphs)   # for nice interactive plot
library(xts)        # dygraphs works with xts-format for timeseries
library(htmlwidgets)


for(i in 2:10){
  dummy_df <- cbind(Pobs[i],Pera[i])
  names(dummy_df) <-  c(paste(colnames(Pobs)[i],"Observed", sep = " "),paste(colnames(Pera)[i],"Predicted", sep = " "))
  Vis <- as.xts(dummy_df, Pobs$Date)
  graph <-  dygraph(Vis, main= paste("ID",colnames(Pobs)[i], sep = " "), ylab = "P in mm")
  saveWidget(graph,paste(colnames(Pobs)[i],".html", sep = ""))
}


boxplot(Pres_year[,2:10])
