# Script to automatically run the sensitivity studies performed in chapter 5.2
# Copyright (C) 2020  Paul Voit

rm(list = ls())

#folders <-  dir()

# delete all old threads/  exe-files
#for (i in folders){
#  setwd (i)
#  unlink("wasa.exe", recursive = TRUE, force = TRUE)
#  setwd("../")
#}

## copy the new Wasa.exe into folders
#for (i in folders){
#  setwd(i)
#  file.copy("../../wasa.exe",getwd())
#  setwd("../")
#}

## Sensitivity study on irrigation rate

setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/AllSensZ3")

curr_obj_fun_val_day <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/LakesZ3/thread1_best/curr_obj_fun_val_day.txt", sep="", skip = 3, header = F)

factors <- c(seq(0.1,3,0.1))
results_rate <-   as.data.frame(curr_obj_fun_val_day$V1)

for (j in 1:length(factors)){
  unlink("thread1")
  # modify irri.dat, multiply rate by factor
  irridat <- read.delim("../AllZ2/init_config/Input/Hillslope/irri.dat", comment.char="#", skip = 1)
  irridat[,c(5:8)] <- irridat[,c(5:8)] * factors[j]
  f <- file("init_config/Input/Hillslope/irri.dat", "w")
  write("# Specification of irrigation operations",f)
  write.table(irridat,file =f, append = TRUE, quote = F,row.names=F,col.names=T,sep="\t", na = "")
  close(f)
  
  source("test_wrapper.R")
  
  obj_fun <- read.table("thread1/curr_obj_fun_val.txt", quote="\"", comment.char="", skip = 3, header = F)
  names(obj_fun)[2] <- factors[i]
  results_rate <- cbind(results_rate, obj_fun[,2])

  write.table(results_rate,"SensRateResults.txt", sep = "\t",  row.names = FALSE, quote = FALSE)
  unlink("thread1")
}

## Sensitivity study on loss factor

#setwd("~/Workspace/RioSaoFrancisco/results_lossCalibration/SensitivityUrucuia/SensLoss")
factor <- seq(0.05,1,0.05)

data.frame("factor" = factors,"rmse_monthly" = 0, "rmse_qtotal" = 0, "NSE"=0)


for (j in 1:length(factors)){
  # modify irri.dat, multiply rate by factor
  irridat <- read.delim("../BaseLineOldIrriData/init_config/Input/Hillslope/irri.dat", comment.char="#")
  irridat[,9] <- factor[j]
  f <- file("init_config/Input/Hillslope/irri.dat", "w")
  write("# Specification of irrigation operations",f)
  write.table(irridat,file =f, append = TRUE, quote = F,row.names=F,col.names=T,sep="\t", na = "")
  close(f)
  
  source("test_wrapper.R")
  
  obj_fun <- read.table("thread1/curr_obj_fun_val.txt", quote="\"", comment.char="", skip = 3)
  results_rate[j,2] <- obj_fun[12,2]
  results_rate[j,3] <- obj_fun[2,2]
  results_rate[j,4] <- obj_fun[10,2]
  
  unlink("thread1")
  
}


## Sensitivity study on crop factor

setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/RLAIF4Folder/SensCropFactor")

factors <- c(seq(0.0005,0.1,0.0005),seq(0.2,1,0.1))
results_rate <-   data.frame("factor" = factors,"rmse_monthly" = 0, "rmse_qtotal" = 0, "NSE"=0, "rmse_lowflow" = 0)

for (j in 1:length(factors)){
  unlink("thread1")
  # modify irri.dat, multiply rate by factor
  irridat <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/RLAIF4Folder/CWD/init_config/Input/Hillslope/irri.dat", header=FALSE, comment.char="#",  skip = 1)
  irridat[,c(5:8)] <- factors[j]
  f <- file("init_config/Input/Hillslope/irri.dat", "w")
  write("# Specification of irrigation operations",f)
  write.table(irridat,file =f, append = TRUE, quote = F,row.names=F,col.names=T,sep="\t", na = "")
  close(f)
  
  source("test_wrapper.R")
  
  obj_fun <- read.table("thread1/curr_obj_fun_val.txt", quote="\"", comment.char="", skip = 3)
  results_rate[j,2] <- obj_fun[12,2]
  results_rate[j,3] <- obj_fun[2,2]
  results_rate[j,4] <- obj_fun[10,2]
  results_rate[j,5] <-  obj_fun[9,2]
  
  
  write.table(results_rate,"SensCropFactorResults.txt", sep = "\t",  row.names = FALSE, quote = FALSE)
  unlink("thread1")
}

