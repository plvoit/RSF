rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

results <- read.delim("ResultsCalibration/SensitivityUrucuia/RLAIF4Folder/SensRateF4/SensRateResults.txt")

par(mar = c(5, 4, 4, 4) + 0.3)  
plot(results$rmse_qtotal~results$factor, type = "l", , xlab = "factor", ylab = expression("RMSE Q "[total]~"[m" ^3*"/s]"), cex.lab = 0.8)
points(results$rmse_qtotal~results$factor, pch = 15)
par(new = T) 
plot(results$rmse~results$factor, type = "l", col = "red", axes = F, xlab = "", ylab = "")
points(results$rmse~results$factor, pch = 15, col = "red")
axis(side = 4, at = pretty(range(results$rmse)))      # Add second axis
mtext( expression("RMSE Q "[low]~"[ m" ^3*"/s]"), side = 4, line = 3, cex = 0.9)   
legend("topright", legend = c(expression("RMSE Q "[total]), expression("RMSE Q "[low]) ), col = c("black", "red"), cex = 0.8, pch = 15)

###  crop factor

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

results <- read.delim("ResultsCalibration/SensitivityUrucuia/RLAIF4Folder/SensCropFactor/SensCropFactorResults.txt")

par(mar = c(5, 4, 4, 4) + 0.3)  
plot(results$rmse_qtotal~results$factor, type = "l", , xlab = expression("crop factor k"[crop]), ylab = expression("RMSE Q "[total]~"[m" ^3*"/s]"), cex.lab = 0.8)
points(results$rmse_qtotal~results$factor, pch = 15, cex = 0.8)
par(new = T) 
plot(results$rmse_lowflow~results$factor, type = "l", col = "red", axes = F, xlab = "", ylab = "")
points(results$rmse_lowflow~results$factor, pch = 15, col = "red", cex = 0.8)
axis(side = 4)      # Add second axis
mtext( expression("RMSE Q "[low]~"[ m" ^3*"/s]"), side = 4, line = 2.3, cex = 0.9)   
legend("bottomright", legend = c(expression("RMSE Q "[total]), expression("RMSE Q "[low]) ), col = c("black", "red"), cex = 0.8, pch = 15)
