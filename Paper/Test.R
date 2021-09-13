rm(list = ls())
irri <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/Z2IrriCal/thread1_best/Output/River_Flow.out", sep="", skip=1)
noirri <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/Z2NoIrriCal/thread1_best/Output/River_Flow.out", sep="", skip=1)
obs <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/Z2IrriCal/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))

noirri <-  noirri[noirri$year < 2010,]

plot(irri$X15~obs$Date, type = "l")
