rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone2newIrri")

act <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone2newIrri/thread1_best/Output/daily_actetranspiration.out", sep="", skip = 1, header = T, check.names = F)
pot <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones/Zone2newIrri/thread1_best/Output/daily_potetranspiration.out", sep="", skip = 1, header = T, check.names = F)

for ( i in 3:ncol(act) ){
  
  plot(pot[,i], type = "l", col = "red")
  lines(act[,i], type = "l")
}


plot(act[,4], type = "l")
