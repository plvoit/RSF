rm(list = ls())

#folders <-  dir()

# run test_wrapper in every folder
#for (i in folders){
#  setwd(i)
#  source("test_wrapper.R")
# GET THE objective function
#  setwd("../")
#}

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

setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/SensRateF4")

factors <- c(seq(0.1,1,0.1),seq(2,10,1),seq(10,100,10))
results_rate <-   data.frame("factor" = factors,"rmse_monthly" = 0, "rmse_qtotal" = 0, "NSE"=0)

for (j in 1:length(factors)){
  unlink("thread1")
 # modify irri.dat, multiply rate by factor
  irridat <- read.delim("../Old/BaseLineOldIrriData/init_config/Input/Hillslope/irri.dat", comment.char="#", skip = 1)
  irridat[,c(5:8)] <- irridat[,c(5:8)] * factors[j]
  f <- file("init_config/Input/Hillslope/irri.dat", "w")
  write("# Specification of irrigation operations",f)
  write.table(irridat,file =f, append = TRUE, quote = F,row.names=F,col.names=T,sep="\t", na = "")
  close(f)
  
  source("test_wrapper.R")
  
  obj_fun <- read.table("thread1/curr_obj_fun_val.txt", quote="\"", comment.char="", skip = 3)
  results_rate[j,2] <- obj_fun[12,2]
  results_rate[j,3] <- obj_fun[2,2]
  results_rate[j,4] <- obj_fun[10,2]
  
  
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
