rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

vegetation <- read.csv("ResultsCalibration/IrriZones/Zone1newIrri/init_config/Input/Hillslope/vegetation.dat", sep="", skip = 1, header = T)

# modify rootdepth
vegetation[,c(9:12)] <- vegetation[,c(9:12)] * 3
# modify LAI
vegetation[,c(13:16)] <- vegetation[,c(13:16)] * 3

f <- file("vegetation.dat", "w")
writeLines("#Specification of vegetation parameters",f)
write.table(vegetation,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)


actMod <- read.csv("ResultsCalibration/Test/thread1/Output/daily_actetranspiration.out", sep="", skip = 1, header = T)
actOld <- read.csv("ResultsCalibration/SensitivityUrucuia/UrucuiaNoIrri/thread1_best/Output/daily_actetranspiration.out", sep="", skip = 1, header = T)


plot(actMod[,3], type = "l")
lines(actOld[,3], type = "l", col = "red")

sum(actMod[,3])
sum(actOld[,3])
