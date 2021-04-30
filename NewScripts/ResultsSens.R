# Copyright (C) 2020  Paul Voit

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

SubbasID_GaugeNumber <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)

results <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/AllSensZ2/Parms1/SensRateResults.txt")
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
for(i in 1:9){
  plot(results[,i]~results$factor, type = "l", main = substr(names(results)[i],13,nchar(names(results)[i])-1),
       ylim = c((min(c(results[,i],results[,i+9])-0.01)),max(c(results[,i],results[,i+9]))), ylab = "normalized reduction of RMSE [%]", xlab = "calibration factor")
  lines(results[,i+9]~results$factor, type = "l", col = "red")
  legend("bottomright", legend= c("RMSE_Qtotal", "RMSE_lowflow"),col = c("black","red"), lty=1, cex=0.8)
  if (saveplots) savePlot(paste("ResultsCalibration/Mod/AllSensZ2/Parms1/SensResults/",substr(names(results)[i],1,nchar(names(results)[i])-1),".png", sep = ""), "png")
}

## for EGU
i = 9
png(file = "~/Workspace/RioSaoFrancisco/Sens96.png", bg = "white", width = 2480, height = 1748, res = 300)
plot(results[,i]~results$factor, type = "l", main = "F_46360000 - RSF km 1105",
     ylim = c((min(c(results[,i],results[,i+9])-0.01)),max(c(results[,i],results[,i+9]))), ylab = "normalized reduction of RMSE [%]", xlab = "calibration factor")
lines(results[,i+9]~results$factor, type = "l", col = "red")
abline(v = results[results[,i] == min(results[,i]),19], lty = 3)
abline(v = results[results[,i+9] == min(results[,i +9 ]),19], lty = 3)
legend("bottomright", legend= c("RMSE_Qtotal", "RMSE_lowflow"),col = c("black","red"), lty=1, cex=1)
text(1.8, 0.975, "min. at calibration factor 0.8 - 1.0", cex = 1.2)
dev.off()

#just look at low flows
results <- results[, c(grep("qtotal", colnames(results)),grep("loflo", colnames(results)))]
results$factor <- c(seq(0.1,3,0.1))

saveplots <- TRUE
windows()
for(i in 1:(ncol(results)-1)){
  plot(results[,i]~results$factor, type = "l", main = names(results)[i])
  if (saveplots) savePlot(paste("ResultsCalibration/Mod/AllSensZ2/Parms1/SensResults/",substr(names(results)[i],1,nchar(names(results)[i])-1),".png", sep = ""), "png")
}




