## Budget

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

vegetation <- read.csv("ResultsCalibration/Test/init_config/Input/Hillslope/vegetation.dat", sep="", skip = 1, header = T)

prec <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/RLAIF4/thread1_best/Input/Time_series/rain_daily.dat", sep="", skip = 2, header = T, check.names = F )
act <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/RLAIF4/thread1_best/Output/actetranspiration.out", sep="", skip = 1, header = T,check.names = F)
pot <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/RLAIF4/thread1_best/Output/potetranspiration.out", sep="", skip = 1, header = T,check.names = F)
mod<- read.table("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/RLAIF4/thread1_best/Output/river_Flow.out", quote="\"", comment.char="", skip = 1, header = T, check.names = F )
obs <-  read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/RLAIF4/init_config/Input/Time_series/discharge_obs_24.txt", header=T, skip = 4)

## AREA Urucuia Subbasin [m2]
area <- 23768011957

## Modelled mod flow to high?
sumMod <- sum(mod[,3])
sumObs <- sum(obs[,19], na.rm = T)

##
diffRiver <-  mod
diffRiver[,3] <- mod[,3] - obs[,19]
summary(diffRiver[,3])



## to mm
diffRiver[,3] <- diffRiver[,3] * 86400 / area * 1000


# Diff
diffET <- pot
diffET[,4] <-  pot[,4] - act[,4]

plot(diffRiver[,3], type = "l")
lines(diffET[,4], type = "l", col = "red")



##yearly mean
sum(pot[,4])/10
sum(act[,4])/10
sum(diffET[,4])/10
sum(diffRiver[,3], na.rm = T)

## obs to mm
sumObs <- sumObs * 86400 / area * 1000
sumMod <- sumMod * 86400 / area * 1000
sumObs/10
sumMod/10

###aggregate the years

diff <- diffET
diff$diffRiver <-  diffRiver[,3]
diff$Timestep <- NULL
names(diff)[3] <-  "diffET"

# set all NA's to zero for aggregating
diff[is.na(diff)] <- 0
diff <- aggregate(diff,list(diff[,1]),sum)
diff[,c(2,3)] <-  NULL

sum(diff$diffET)


# Precipitation yearly mean

PrecSum <-  sum(prec$`15`)/10
