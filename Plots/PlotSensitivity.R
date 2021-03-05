rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

results <- read.delim("ResultsCalibration/SensitivityUrucuia/RLAIF4Folder/SensRateF4/SensRateResults.txt")
results$red_lowflow <- 100 - results$rmse/max(results$rmse) * 100
results$red_qtotal <- 100 - results$rmse_qtotal /max(results$rmse_qtotal) * 100

## Tills version, normalized plots
png(file = "~/Workspace/RioSaoFrancisco/SensitivityRateRed.png", bg = "white", width = 2480, height = 1748, res = 300)
par(mar = c(5, 4, 4, 4) + 0.3)  
plot(results$red_lowflow~results$factor, type = "l",  xlab = "factor", ylim = c(0,10), ylab = expression("Normalized reduction of RMSE [%]"), cex.lab = 0.8,  col = "red")
points(results$red_lowflow~results$factor, pch = 15, col = "red")
lines(results$red_qtotal~results$factor, type = "l")
points(results$red_qtotal~results$factor, pch = 15)
legend("topleft", legend = c(expression("RMSE Q "[total]), expression("RMSE Q "[low]) ), col = c("black", "red"), cex = 0.8, pch = 15)
dev.off()


png(file = "~/Workspace/RioSaoFrancisco/SensitivityRate.png", bg = "white", width = 2480, height = 1748, res = 300)
par(mar = c(5, 4, 4, 4) + 0.3)  
plot(results$rmse_qtotal~results$factor, type = "l",  xlab = "factor", ylab = expression("RMSE Q "[total]~"[m" ^3*"/s]"), cex.lab = 0.8)
points(results$rmse_qtotal~results$factor, pch = 15)
par(new = T) 
plot(results$rmse~results$factor, type = "l", col = "red", axes = F, xlab = "", ylab = "")
points(results$rmse~results$factor, pch = 15, col = "red")
axis(side = 4, at = pretty(range(results$rmse)))      # Add second axis
mtext( expression("RMSE Q "[low]~"[ m" ^3*"/s]"), side = 4, line = 3, cex = 0.9)   
legend("topright", legend = c(expression("RMSE Q "[total]), expression("RMSE Q "[low]) ), col = c("black", "red"), cex = 0.8, pch = 15)
dev.off()
###  crop factor

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

results <- read.delim("ResultsCalibration/SensitivityUrucuia/RLAIF4Folder/SensCropFactor/SensCropFactorResults.txt")
results$red_lowflow <- 100 - results$rmse_lowflow/max(results$rmse_lowflow) * 100
results$red_qtotal <- 100 - results$rmse_qtotal /max(results$rmse_qtotal) * 100

## normalized
png(file = "~/Workspace/RioSaoFrancisco/SensitivityCropRed.png", bg = "white", width = 2480, height = 1748, res = 300)
par(mar = c(5, 4, 4, 4) + 0.3)  
plot(results$red_lowflow~results$factor, type = "l",  xlab = "factor", ylab = expression("Normalized reduction of RMSE [%]"), cex.lab = 0.8,  col = "red")
points(results$red_lowflow~results$factor, pch = 15, col = "red")
lines(results$red_qtotal~results$factor, type = "l")
points(results$red_qtotal~results$factor, pch = 15)
legend("topright", legend = c(expression("RMSE Q "[total]), expression("RMSE Q "[low]) ), col = c("black", "red"), cex = 0.8, pch = 15)
dev.off()

png(file = "~/Workspace/RioSaoFrancisco/SensCrop.png", bg = "white", width = 2480, height = 1748, res = 300)
par(mar = c(5, 4, 4, 4) + 0.3)  
plot(results$rmse_qtotal~results$factor, type = "l", xlab = expression("crop factor k"[crop]), ylab = expression("RMSE Q "[total]~"[m" ^3*"/s]"), cex.lab = 0.8)
points(results$rmse_qtotal~results$factor, pch = 15, cex = 0.8)
par(new = T) 
plot(results$rmse_lowflow~results$factor, type = "l", col = "red", axes = F, xlab = "", ylab = "")
points(results$rmse_lowflow~results$factor, pch = 15, col = "red", cex = 0.8)
axis(side = 4)      # Add second axis
mtext( expression("RMSE Q "[low]~"[ m" ^3*"/s]"), side = 4, line = 2.3, cex = 0.9)   
legend("bottomright", legend = c(expression("RMSE Q "[total]), expression("RMSE Q "[low]) ), col = c("black", "red"), cex = 0.8, pch = 15)
dev.off()
