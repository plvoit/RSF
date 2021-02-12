rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration") 

#savePlot = T

#### Zone 1
IrriZone1 <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone1newIrri/thread1_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
Obs <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone1newIrri/thread1_best/Input/Time_series/discharge_obs_24.txt", sep="", skip = 4, header = T, check.names = F)
Prec <- read.csv("SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Input/Time_series/rain_daily.dat", sep="", skip = 2, header = T, check.names = F)
NoIrriZone1 <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/NoIrriZones/Zone1new/thread1_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F) 
Obs$Date <-  paste(Obs$YYYY,Obs$MM,Obs$DD, sep = "-")
Obs$Date <- as.POSIXct(Obs$Date, format = c("%Y-%m-%d"))

# load clearer names from CheckWASAoutput.R for later labelling the plots
# load GaugeNumber-SubbasID file
SubbasID_GaugeNumber <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)

subbas_id = c("10","11","12") #Zone 1
#subbas_id=c("78","73","15","16","90","58","96","45") #Zone2
#subbas_id = c("1","2","3") #Zone3
#subbas_id = c("15")

Obs <- Obs[,c(ncol(Obs), match(subbas_id,colnames(Obs)))]
IrriZone1 <- IrriZone1[,c(1,2,match(subbas_id,colnames(IrriZone1)))]

#windows()
par(mfrow=c(3,1))
par(mar = c(3.5,4,2,5))
for ( i in 3:ncol(IrriZone1) ){
barplot(Prec[,match(colnames(IrriZone1)[i],colnames(Prec))], ylim = rev(c(0,120)),yaxt = "n",
        col = rep("dodgerblue",nrow(Prec)), border = "dodgerblue", main = SubbasID_GaugeNumber[match(colnames(IrriZone1)[i],SubbasID_GaugeNumber$Subbas_ID),3], cex.main = 1.2)
axis(4, cex.axis = 1, at = seq(0,120,40))
mtext("Precipitation [mm]", 4, line = 2, cex.axis = 1, cex = 0.8)
par(new = TRUE)
plot(IrriZone1[,i]~Obs$Date, type = "l", col = "red", ylim = c(0,3500), ylab = "",xlab = "",cex.lab = 0.9, cex.axis = 1)
title(ylab = expression("Runoff [m" ^3*"/s]"), line = 2, cex.lab = 1, xlab = "Year")
lines(Obs[,match(colnames(IrriZone1)[i],colnames(Obs))]~Obs$Date, type = "l")
#lines(NoIrriZone1[,match(colnames(IrriZone1)[i],colnames(NoIrriZone1))]~Obs$Date, type = "l", col = "red")
#legend("right", legend = c("Modelled", "Observed", "Precipitation"), col = c("red", "black", "blue"), cex = 0.8,  lty=1)
box(bty = "7", lwd = 2)
}
# if(savePlot) savePlot(paste("Zone1All.png"))

#####################################
## Zone 2
IrriZone2 <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone2newIrri/thread1_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
Obs <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone2newIrri/thread1_best/Input/Time_series/discharge_obs_24.txt", sep="", skip = 4, header = T, check.names = F)
Prec <- read.csv("SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Input/Time_series/rain_daily.dat", sep="", skip = 2, header = T, check.names = F)
NoIrriZone2 <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/NoIrriZones/Zone2new/thread1_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F) 

Obs$Date <-  paste(Obs$YYYY,Obs$MM,Obs$DD, sep = "-")
Obs$Date <- as.POSIXct(Obs$Date, format = c("%Y-%m-%d"))

# load clearer names from CheckWASAoutput.R for later labelling the plots
# load GaugeNumber-SubbasID file
SubbasID_GaugeNumber <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)

subbas_id=c("78","73","15","16","90","58","96","45") #Zone2


Obs <- Obs[,c(ncol(Obs), match(subbas_id,colnames(Obs)))]
IrriZone2 <- IrriZone2[,c(1,2,match(subbas_id,colnames(IrriZone2)))]


par(mfrow=c(3,1))
par(mar = c(3.5,4,2,5))
for ( i in 3:ncol(IrriZone2) ){
  barplot(Prec[,match(colnames(IrriZone2)[i],colnames(Prec))], ylim = rev(c(0,120)),yaxt = "n",
          col = rep("dodgerblue",nrow(Prec)), border = "dodgerblue", main = SubbasID_GaugeNumber[match(colnames(IrriZone2)[i],SubbasID_GaugeNumber$Subbas_ID),3], cex.main = 1.2)
  axis(4, cex.axis = 1, at = seq(0,120,40))
  mtext("Precipitation [mm]", 4, line = 2, cex.axis = 1, cex = 0.8)
  par(new = TRUE)
  plot(IrriZone2[,i]~Obs$Date, type = "l", col = "green", ylim = c(0,max(IrriZone2[,i])+1000), ylab = "",xlab = "",cex.lab = 0.9, cex.axis = 1)
  title(ylab = expression("Runoff [m" ^3*"/s]"), line = 2, cex.lab = 1, xlab = "Year")
  lines(Obs[,match(colnames(IrriZone2)[i],colnames(Obs))]~Obs$Date, type = "l")
  lines(NoIrriZone2[,match(colnames(IrriZone2)[i],colnames(NoIrriZone2))]~Obs$Date, type = "l", col = "red")
  #legend("right", legend = c("Modelled", "Observed", "Precipitation"), col = c("red", "black", "blue"), cex = 0.8,  lty=1)
  box(bty = "7", lwd = 2)
}





#####################################
## Zone 3
IrriZone3 <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone3newIrri/thread1_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
Obs <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone3newIrri/thread1_best/Input/Time_series/discharge_obs_24.txt", sep="", skip = 4, header = T, check.names = F)
Prec <- read.csv("SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Input/Time_series/rain_daily.dat", sep="", skip = 2, header = T, check.names = F)
NoIrriZone3 <- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/NoIrriZones/Zone3new/thread1_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F) 
Obs$Date <-  paste(Obs$YYYY,Obs$MM,Obs$DD, sep = "-")
Obs$Date <- as.POSIXct(Obs$Date, format = c("%Y-%m-%d"))

# load clearer names from CheckWASAoutput.R for later labelling the plots
# load GaugeNumber-SubbasID file
SubbasID_GaugeNumber <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)

subbas_id=c("1","2","3") #Zone2


Obs <- Obs[,c(ncol(Obs), match(subbas_id,colnames(Obs)))]
IrriZone3 <- IrriZone3[,c(1,2,match(subbas_id,colnames(IrriZone3)))]

par(mfrow=c(3,1))
par(mar = c(3.5,4,2,5))
for ( i in 3:ncol(IrriZone3) ){
  barplot(Prec[,match(colnames(IrriZone3)[i],colnames(Prec))], ylim = rev(c(0,120)),yaxt = "n",
          col = rep("dodgerblue",nrow(Prec)), border = "dodgerblue", main = SubbasID_GaugeNumber[match(colnames(IrriZone3)[i],SubbasID_GaugeNumber$Subbas_ID),3], cex.main = 1.2)
  axis(4, cex.axis = 1, at = seq(0,120,40))
  mtext("Precipitation [mm]", 4, line = 2, cex.axis = 1, cex = 0.8)
  par(new = TRUE)
  plot(IrriZone3[,i]~Obs$Date, type = "l", col = "green", ylim = c(0,max(Obs[,match(colnames(IrriZone3)[i],colnames(Obs))], na.rm = T)+2000), ylab = "",xlab = "",cex.lab = 0.9, cex.axis = 1)
  title(ylab = expression("Runoff [m" ^3*"/s]"), line = 2, cex.lab = 1, xlab = "Year")
  lines(Obs[,match(colnames(IrriZone3)[i],colnames(Obs))]~Obs$Date, type = "l")
  lines(NoIrriZone3[,match(colnames(IrriZone3)[i],colnames(NoIrriZone3))]~Obs$Date, type = "l", col = "red")
  #legend("right", legend = c("Modelled", "Observed", "Precipitation"), col = c("red", "black", "blue"), cex = 0.8,  lty=1)
  box(bty = "7", lwd = 2)
}



