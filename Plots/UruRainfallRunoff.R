rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration")

UruMod <- read.csv("SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Output/River_Flow.out", sep="", skip = 1, header = T)
UruObs <- read.csv("SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Input/Time_series/discharge_obs_24.txt", sep="", skip = 4, header = T)
Prec <- read.csv("SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Input/Time_series/rain_daily.dat", sep="", skip = 2, header = T)

UruObs$Date <-  paste(UruObs$YYYY,UruObs$MM,UruObs$DD, sep = "-")
UruObs$Date <- as.POSIXct(UruObs$Date, format = c("%Y-%m-%d"))

Uru <- data.frame("Date" = UruObs$Date, "Obs" = UruObs$X15, "Mod" = UruMod$X15, "Prec" = Prec$X15)


par(mai = c(1, 1, 1, 1))
barplot(Uru$Prec, ylim = rev(c(0,120)), yaxt = "n", col = rep( "dodgerblue", length(Uru$Prec)), border = "dodgerblue")
mtext("Precipitation [mm]", 4, line = 3, cex = 0.8)
axis(4, cex.axis = 0.8)

par(new = TRUE)
plot(Uru$Mod~Uru$Date, type = "l", col = "red", ylim = c(0,3500), cex.axis = 0.8, ylab = expression("Runoff [m" ^3*"]"),
     xlab = "Year", cex.lab = 0.8, cex.axis = 0.8)
lines(Uru$Obs~Uru$Date, type = "l")
legend("right", legend = c("Modelled", "Observed","Precipitation"), col = c("red", "black", "blue"), cex = 0.8,  lty=1)
box(bty = "7", lwd = 2)


ETpot <- read.csv("SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Output/daily_potetranspiration.out", sep="", skip = 1, header = T)
ETact<- read.csv("SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Output/daily_actetranspiration.out", sep="", skip = 1, header = T)

plot(ETpot$X15~Uru$Date, type = "l", xlab = "Year", ylab = "[mm]", ylim = c(0,13), cex.axis = 0.8, cex.lab = 0.8)
lines(ETact$X15~Uru$Date, type = "l", col = "red")
legend("topright", legend = c(expression("ET"[act]),expression("ET"[pot])), col = c("red","black"), cex = 0.8, lty = 1)
box()
