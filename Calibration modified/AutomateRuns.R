rm(list = ls())
#setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/SensLoss")
#setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/SensRate")


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


setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/Test")


factors <- c(seq(0.1,1,0.1),seq(2,10,1),seq(10,100,10))
results <-   data.frame("factor" = factors)

for (i in factors){
 # modify irri.dat, multiply rate by factor
  irridat <- read.delim("../BaseLineOldIrriData/init_config/Input/Hillslope/irri.dat", comment.char="#")
  irridat[,c(5:8)] <- irridat[,c(5:8)] * i
  f <- file("init_config/Input/Hillslope/irri.dat", "w")
  write("# Specification of irrigation operations",f)
  write.table(irridat,file =f, append = TRUE, quote = F,row.names=F,col.names=T,sep="\t", na = "")
  close(f)
  
  source("test_wrapper.R")
  
  obj_fun <- read.table("thread1/curr_obj_fun_val.txt", quote="\"", comment.char="", skip = 3)
  results$rmse_monthly <- obj_fun[12,2]
  results$rmse_qtotal <- obj_fun[2,2]
  results$NSE <- obj_fun[10,2]
  
  unlink("thread1")
  
}


