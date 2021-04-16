# Copyright (C) 2020  Paul Voit

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

results <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/AllSensZ2/SensRateResults.txt")
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

#just look at low flows
results <- results[, grep("loflo", colnames(results))]
results$factor <- c(seq(0.1,3,0.1))

for(i in 1:(ncol(results)-1)){
  plot(results[,i]~results$factor, type = "l", main = names(results)[i])
}
