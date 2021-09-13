rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe")

runs <- list.dirs(recursive = F, full.names = T)
runs <- runs[8]
#  
# for( i in runs){
#   setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper")
#   setwd(i)
#   #remove the do.dat and put the one that has irrigation = F, add paramset.txt
#   # to run testwrapper.R (wasa with warmup)
#   file.remove('init_config/Input/do.dat')
#   file.copy('../do.dat','init_config/Input')
#   file.copy('thread1_best/paramset.txt', 'init_config')
#   source('test_wrapper.R')
#   file.rename('thread1','NoIrri')
#   }



for(i in runs){
  setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe")
  setwd(i)
  
  #use the right subbasins to plot
  if(grepl('Cari',i)) subs <- '16'
  if(grepl('Grande',i)) subs <- '45'
  if(grepl('Uru',i)) subs <- '15'
  if(grepl('Z2',i)) subs <- c('13','73','78','15','16','90','58','45','96')
  
  #plot and save
  irri <- read.table("Irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
  obs <- read.delim("init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
  noirri <- read.table('NoIrri/Output/River_Flow.out', quote="\"", comment.char="",skip = 1, header = T, check.names = F)

  obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
  obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))
  
  obs <-  obs[obs$Date < "2015-01-01",]
  
  
  for (j in subs){
    png(file = paste0(j, "_irriNoirri.png"), bg = "white", width = 2480, height = 1748, res = 300)
    plot(obs[,j]~obs$Date, type = "l", main = paste0('Sub ',j), xlab = 'Date', ylab = expression(m^3*'/s'))
    lines(noirri[,j]~obs$Date, type = 'l', col = 'red')
    lines(irri[,j]~obs$Date, type = "l", col = "green")
    legend("topright", legend= c("observed", "modeled (no irrigation)","modeled (irrigation)"),col = c("black","red", "green"), lty=1, cex=0.8)
    dev.off()
    
   
  }
}

