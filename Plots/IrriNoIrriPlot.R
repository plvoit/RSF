rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones")

thread <- "Zone2newIrri"  ## Check right subbas_id in line 11!
saveplots <- T
noirri_thread <- "../NoIrriZones/Zone2new"
# load clearer names from CheckWASAoutput.R for later labelling the plots
# load GaugeNumber-SubbasID file
SubbasID_GaugeNumber <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)

#subbas_id = c("10","11","12") #Zone 1
subbas_id=c("78","73","15","16","90","58","96","45") #Zone2
#subbas_id = c("1","2","3") #Zone3
#subbas_id = c("15")


obs <- read.delim(paste(thread,"/thread1_best/Input/Time_series/discharge_obs_24.txt", sep = "") , header= T, skip = 4, check.names  = F)
mod_irri <- read.table(paste(thread,"/thread1_best/Output/River_Flow.out", sep = ""), quote="\"", comment.char="", skip = 1, header = T, check.names = F)
mod_noirri <- read.table(paste(noirri_thread,"/thread1_best/Output/River_Flow.out", sep = ""), quote="\"", comment.char="", skip = 1, header = T, check.names = F)

obs <- obs[,c(1:4, match(subbas_id,colnames(obs)))]
mod_irri <- mod_irri[,c(1:4, match(subbas_id,colnames(mod_irri)))]
mod_noirri <- mod_noirri[,c(1,2,match(subbas_id,colnames(mod_noirri)))]

obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))

windows()
for (i in subbas_id){
  
  plot(obs[[i]], type = "l", main = SubbasID_GaugeNumber[match(i,SubbasID_GaugeNumber$Subbas_ID),3], xlab = "days", ylab = "m3/s")
  lines(mod_noirri[[i]], type = "l", col = "red")
  lines(mod_irri[[i]], type = "l", col = "green")
  legend("topright", legend= c("observed", "modeled (no irrigation)","modeled (irrigation)"),col = c("black","red", "green"), lty=1, cex=0.8)
  if (saveplots) savePlot(paste("IrriNoIrri_",SubbasID_GaugeNumber[match(i,SubbasID_GaugeNumber$Subbas_ID),3],".png", sep = ""), "png")
}

i = "90"
png(file = "~/Workspace/RioSaoFrancisco/IrriNoIrri.png", bg = "white", width = 2480, height = 1748, res = 300)
plot(mod_noirri[[i]]~obs$Date, type = "l", col = "red",xlab = "Year", ylab = "m3/s")
lines(obs[[i]]~obs$Date, type = "l")
lines(mod_irri[[i]]~obs$Date, type = "l", col = "green")
legend("topright", legend= c("observed", "modeled (no irrigation)","modeled (irrigation)"),col = c("black","red", "green"), lty=1, cex=0.8)
dev.off()

