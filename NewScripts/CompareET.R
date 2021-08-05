rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

#load old Settings
old <- read.csv("ResultsCalibration/Mod/AllZ2/thread1_best/Output/daily_actetranspiration.out", sep="", skip = 1)

#changed alluvial settings (maxdep in soter.dat)
alluv <- read.csv("ResultsCalibration/Alluv/Z2/thread1_best/Output/daily_actetranspiration.out", sep="", skip = 1)

# ETact for all Zone
sum_old <- sum(old[,3:ncol(old)])
sum_alluv <- sum(alluv[,3:ncol(alluv)])

for( i in 3:ncol(old)){
  plot(old[,i], type = "l")
  lines(alluv[,i], type = "l", col = "red")
}
