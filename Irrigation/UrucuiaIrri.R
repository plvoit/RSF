rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(foreign)

Irri <- read.dbf("GIS/Urucuia/UrucuiaWaterAbstraction.dbf")

# calculate the daily volume of water abstracted (m^3/d)
Irri$Jan <- (Irri[,19] * Irri[,20] * Irri[,21])/ 31
Irri$Feb <- (Irri[,22] * Irri[,23] * Irri[,24])/ 28
Irri$Mar <- (Irri[,25] * Irri[,26] * Irri[,27])/ 31
Irri$Apr <- (Irri[,28] * Irri[,29] * Irri[,30])/ 30
Irri$May <- (Irri[,31] * Irri[,32] * Irri[,33])/ 31
Irri$Jun <- (Irri[,34] * Irri[,35] * Irri[,36])/ 30
Irri$Jul <- (Irri[,37] * Irri[,38] * Irri[,39])/ 31
Irri$Aug <- (Irri[,40] * Irri[,41] * Irri[,42])/ 31
Irri$Sep <- (Irri[,43] * Irri[,44] * Irri[,45])/ 30
Irri$Oct <- (Irri[,46] * Irri[,47] * Irri[,48])/ 31
Irri$Nov <- (Irri[,49] * Irri[,50] * Irri[,51])/ 30
Irri$Dec <- (Irri[,52] * Irri[,53] * Irri[,54])/ 31

MonthlyIrri <- c()

j = 1
for (i in 68:79){
  MonthlyIrri[j] <- sum(Irri[,i])
  j = j + 1
}

names(MonthlyIrri) <-  c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov", "Dec")
season <- data.frame("Doy" = c(3,4,8,11), "Rate" = MonthlyIrri[c(3,4,8,11)])

barplot(MonthlyIrri, cex.names = 0.9, ylab = expression("irrigation water [m"^3* "/d]") , col = "#3366FF", main = "Irrigation in the Urucuia subbasin")
lines(season$Rate~season$Doy, type = "l")

# constant rate

Day <-  c(31,28,31,30,31,30,31,31,30,31,30,31)

sum(MonthlyIrri * Day)/365


## graphs for sensitivity study
results <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/SensitivityUrucuia/SensRate/SensRateResults.txt")

plot(results$rmse_qtotal~results$factor, type = "l", col = "red", ylab = expression("RMSE Q"[total]* " [m"^3*"/s]"), xlab = "rate factor", cex.lab = 0.8 )
points(results$rmse_qtotal~results$factor, col = "red", pch = 15)



### For Tills Fit-script
DF <- data.frame("doy" = seq(1,365,1), "Irri" = 0)
DF$Irri <- c(rep(MonthlyIrri[1],Day[1]),
             rep(MonthlyIrri[2],Day[2]),
             rep(MonthlyIrri[3],Day[3]),
             rep(MonthlyIrri[4],Day[4]),
             rep(MonthlyIrri[5],Day[5]),
             rep(MonthlyIrri[6],Day[6]),
             rep(MonthlyIrri[7],Day[7]),
             rep(MonthlyIrri[8],Day[8]),
             rep(MonthlyIrri[9],Day[9]),
             rep(MonthlyIrri[10],Day[10]),
             rep(MonthlyIrri[11],Day[11]),
             rep(MonthlyIrri[12],Day[12]))
DF$Irri <- round(DF$Irri,0)

write.table(DF,file ="Irri_season.dat", sep = "\t", row.names = FALSE, quote = FALSE)
