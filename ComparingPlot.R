### This script visualizes the results of the Check (downscaling,ERA5,WatchERA5) -scripts


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")


Observed <- read.csv("~/Workspace/RioSaoFrancisco/Data/Monthly means/Observed.txt")
ERA5 <- read.csv("~/Workspace/RioSaoFrancisco/Data/Monthly means/Predicted_ERA5.txt")
WatchERA5 <- read.csv("~/Workspace/RioSaoFrancisco/Data/Monthly means/Predicted_WatchERA5.txt")
Predicted_downscaling <- read.csv("~/Workspace/RioSaoFrancisco/Data/Monthly means/Predicted_downscaling.txt")

plot(Observed$Mmo_total~Observed$Month,type = "l", ylab= "mean P [mm]", main ="Mean monthly P for all 65 stations",
     xlab = "Month", lwd = 2, ylim = c(0,200))
lines(ERA5$Mmp_total~Observed$Month, type = "l", col = "red" )
lines(WatchERA5$Mmp_total~Observed$Month, type = "l", col = "blue") ## sind identisch??
lines(Predicted_downscaling$Mmp_total~Observed$Month, type = "l", col= "green")
legend("topright", legend = c("Observed","ERA5","WatchERA5","Downscaling"), col = c("black","red", "blue", "green"), lty=1, cex=0.8)


#  plot(Pobs[,i]~Pobs$Date, type = "l", ylab= "P", main =paste(colnames(Pobs)[i],"Daily Precipitation", sep = " "))
#  lines(Pera[,i]~Pera$Date, type = "l", col = "red")
# legend("topright", legend = c("Observed","Hindcast"), col = c("black","red"), lty=1, cex=0.8)
# }
