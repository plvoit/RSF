# Copyright (C) 2020  Paul Voit

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

results <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/AllSensZ1/SensRateResults.txt")
results[2:length(results)] <- round(results[2:length(results)],2) 
names(results)[2:ncol(results)] <- c(seq(0.1,3,0.1))
results <-t(results)
results <- as.data.frame(results, stringsAsFactors = F)

for(i in 1:ncol(results)) names(results)[i] <- results[1,i]
results <- results[-1,]
results[is.na(results)] <- 0

results <- results[,-grep("bias", colnames(results))]
results <- results[,-grep("penalty", colnames(results))]
results <- results[,-grep("rmse_qbas", colnames(results))]



for(i in 1:ncol(results)) results[,i] <- as.numeric(results[,i])
results$factor <- c(seq(0.1,3,0.1))


results <- results[, c(grep("qtotal", colnames(results)),grep("loflo", colnames(results)),grep("factor", colnames(results)))]

#normalize, so they can be plotted in one plot
for(i in 1:(ncol(results)-1)){
  results[,i] <-  sapply(results[,i], function(x) x <- round(x/max(results[,i], na.rm = T),4))
}

#plot rmse_qtotal and  rmse_loflo in one graph
saveplots <- T
windows()
for(i in 1:7){
  plot(results[,i]~results$factor, type = "l", main = substr(names(results)[i],13,nchar(names(results)[i])-1), ylim = c((min(c(results[,i],results[,i+3])-0.01)),max(c(results[,i],results[,i+3]))))
  lines(results[,i+3]~results$factor, type = "l", col = "red")
  legend("bottomright", legend= c("RMSE_Qtotal", "RMSE_lowflow"),col = c("black","red"), lty=1, cex=0.8)
  if (saveplots) savePlot(paste("ResultsCalibration/Mod/AllSensZ1/SensResults/",substr(names(results)[i],1,nchar(names(results)[i])-1),".png", sep = ""), "png")
}



#just look at low flows
results <- results[, c(grep("qtotal", colnames(results)),grep("loflo", colnames(results)))]
results$factor <- c(seq(0.1,3,0.1))

saveplots <- TRUE
windows()
for(i in 1:(ncol(results)-1)){
  plot(results[,i]~results$factor, type = "l", main = names(results)[i])
  if (saveplots) savePlot(paste("ResultsCalibration/Mod/AllSensZ2/Parms1/SensResults/",substr(names(results)[i],1,nchar(names(results)[i])-1),".png", sep = ""), "png")
}


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
### New Approach
## Irri Results
Z1I <- read.table("ResultsCalibration/Mod/AllZ1/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(Z1I) <- c("obj_fun","val")
Z2I <- read.delim("ResultsCalibration/Mod/AllSensZ2/Parms1/SensRateResults.txt", header=F)
Z2I <- Z2I[2:nrow(Z2I),c(1,11)]#
names(Z2I) <- c("obj_fun","val")
Z3I <- read.table("ResultsCalibration/Mod/AllZ3/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(Z3I) <- c("obj_fun","val")

AllI <- rbind(Z1I,Z2I,Z3I)

#Lake results
L1 <- read.table("ResultsCalibration/Mod/LakesZ1/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(Z1I) <- c("obj_fun","val")
L2 <- read.table("ResultsCalibration/Mod/LakesZ2/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(Z2I) <- c("obj_fun","val")
L3 <- read.table("ResultsCalibration/Mod/LakesZ3/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(Z3I) <- c("obj_fun","val")

AllL <- rbind(L1,L2,L3)

#modifified vegetation results

M1 <- read.table("ResultsCalibration/Mod/CalVegZ1/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M1) <- c("obj_fun","val")
M2 <- read.table("ResultsCalibration/Mod/CalVegZ2/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M2) <- c("obj_fun","val")
M3 <- read.table("ResultsCalibration/Mod/CalVegZ3/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M3) <- c("obj_fun","val")

MAll <- rbind(M1,M2,M3)

#Original Parameters

O1 <- read.table("ResultsCalibration/NoIrriZones/Zone1new/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M1) <- c("obj_fun","val")
O2 <- read.table("ResultsCalibration/NoIrriZones/Zone2new/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M2) <- c("obj_fun","val")
O3 <- read.table("ResultsCalibration/NoIrriZones/Zone3new/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M3) <- c("obj_fun","val")

OAll <- rbind(O1,O2,O3)

#join all of the to one dataframe

All <- cbind(OAll,MAll$val,AllL$V2,AllI$val)
names(All) <- c("obj_fun","Original","mod_veg","Lakes","Irri")
#All[is.na(All)] <- 0
All$Irri <- as.numeric(All$Irri)

#take out unnecessary rows
All <- All[-(grep("sed_bias",All[,1])),]
All <- All[-(grep("penalty",All[,1])),]
All <- All[-(grep("rmse_qbas",All[,1])),]

for(i in 1:nrow(All)){
  dummy <- t(All[i,c(2:5)])
  plot(dummy, type = "l", main = All[i,1])
}  
