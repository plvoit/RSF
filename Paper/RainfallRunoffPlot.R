###### Pot Irri NoIrri Cal
#plot and save
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs")

irri <- read.table("Z2Irri/irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
obs <- read.delim("Z2Irri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
noirri <- read.csv("Z2NoIrriNew/thread1_best/Output/River_Flow.out", sep="", skip=1, header = T, check.names = F)

prec <- read.csv("Z2NoIrriNew/thread1_best/Input/Time_series/rain_daily.dat", sep="", skip = 2, header = T, check.names = F )

obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))

obs <-  obs[obs$Date < "2015-01-01",]

prec <- prec[1:nrow(obs),]

# dataframe for renaming the subbasin ID for paper indexes
subID <- c('13','73','78','15','16','90','58','45','96')
#PaperID <- c('11', '12', '4', '13', '14', '3', '2', '15', '1')

#noirri <- noirri[noirri$year < 2010,]
#use the right subbasins to plot
if(grepl('Cari',thread)) subs <- '16'
if(grepl('Grande',thread)) subs <- '45'
if(grepl('Uru',thread)) subs <- '15'
if(grepl('Z2',thread)) subs <- c('13','73','78','15','16','90','58','45','96')



#### RUNOFF PLOTS

for (i in subs){
  
  df <- data.frame("Date" = obs$Date, "irri"=irri[,match(i,colnames(irri))], "Noirri"=noirri[,match(i,colnames(noirri))],
                   "Prec" = prec[,match(i,colnames(prec))], "Obs" = obs[,match(i,colnames(obs))])
  png(file = paste0(i, "_irriNoirri.png"), bg = "white", width = 2480, height = 1748, res = 300)
  plot(df$Obs~df$Date, type = "l", ylim = c(0,max(df$irri, df$Noirri, df$Obs, na.rm = T)), cex.axis = 0.8, ylab = expression("discharge [m" ^3*"/s]"),
       xlab = "Year", cex.lab = 0.8, cex.axis = 0.8, main = paste0('streamflow gauge ', i))
  lines(noirri[,i]~obs$Date, type = 'l', col = 'red')
  lines(irri[,i]~obs$Date, type = "l", col = "green")
  legend("topright", legend= c("observed", "modeled (no irrigation)","modeled (irrigation)"),col = c("black","red", "green"), lty=1, cex=0.8)
  dev.off()
}

## multiple plots in one graphic
subs <- c('58', '96')
years <- as.POSIXct(c('2000-01-01', '2001-01-01','2002-01-01','2003-01-01', '2004-01-01', '2005-01-01', '2006-06-01', '2007-01-01', '2008-01-01',
                      '2009-01-01','2010-01-01','2011-01-01','2012-01-01','2012-01-01','2014-01-01', '2015-01-01'))


  png(file = "Multiplot_irriNoirri.png", bg = "white", width = 2480, height = 1748, res = 300)
  par(mfrow = c(2, 1), mai = c(0.3,0.5,0.5, 0.1), mar = numeric(4), oma = c(4, 4, .5, .5), 
      mgp = c(2, .6, 0))


  df <- data.frame("Date" = obs$Date, "irri"=irri[,match(subs[1],colnames(irri))], "Noirri"=noirri[,match(subs[1],colnames(noirri))],
                   "Prec" = prec[,match(subs[1],colnames(prec))], "Obs" = obs[,match(subs[1],colnames(obs))])
  
  plot(df$Obs~df$Date, type = "l", ylim = c(0,max(df$irri, df$Noirri, df$Obs, na.rm = T)), cex.axis = 0.8, ylab = expression("discharge [m" ^3*"/s]"),
       cex.lab = 0.8, cex.axis = 0.8, xaxt = 'n', xlab ="")
  lines(df$Noirri~df$Date, type = 'l', col = 'red')
  lines(df$irri~df$Date, type = "l", col = "green")
  abline(h=seq(2000,10000,2000), lty = 2, col = "grey")
  abline(v= years, lty = 2, col = "grey")
  #legend("topright", legend= c("observed", "modeled (no irrigation)","modeled (irrigation)"),col = c("black","red", "green"), lty=1, cex=0.8)
  
  # second plot
  df <- data.frame("Date" = obs$Date, "irri"=irri[,match(subs[2],colnames(irri))], "Noirri"=noirri[,match(subs[2],colnames(noirri))],
                   "Prec" = prec[,match(subs[2],colnames(prec))], "Obs" = obs[,match(subs[2],colnames(obs))])
  plot(df$Obs~df$Date, type = "l", ylim = c(0,max(df$irri, df$Noirri, df$Obs, na.rm = T)), cex.axis = 0.8, ylab = expression("discharge [m" ^3*"/s]"),
       xlab = "Year", cex.lab = 0.8, cex.axis = 0.8)
  axis.Date(1, at = c('2000-01-01', '2002-01-01'),
            format= "%Y")
  lines(df$Noirri~df$Date, type = 'l', col = 'red')
  lines(df$irri~df$Date, type = "l", col = "green")
  abline(h=seq(1000,5000,1000), lty = 2, col = "grey")
  abline(v= years, lty = 2, col = "grey")
  legend("topright", legend= c("observed", "modeled (no irrigation)","modeled (irrigation)"),col = c("black","red", "green"), lty=1, cex=0.8)
  mtext("Year", side = 1, outer = TRUE, line = 2.2)
  mtext(expression("Q [m" ^3*"/s]"), side = 2, outer = TRUE, line = 2.2)
  
  dev.off()
  

#### RAINFALL RUNOFF for Rio Urucuia

  ###### Pot Irri NoIrri Cal
  #plot and save
  rm(list = ls())
  setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs")
  thread <- "Uru"
  
  years <- as.POSIXct(c('2000-01-01', '2001-01-01','2002-01-01','2003-01-01', '2004-01-01', '2005-01-01', '2006-06-01', '2007-01-01', '2008-01-01',
                        '2009-01-01','2010-01-01','2011-01-01','2012-01-01','2012-01-01','2014-01-01', '2015-01-01'))
  
  irri <- read.table(paste0(thread,"IrriGW/irri_best/Output/River_Flow.out"), quote="\"", comment.char="",skip = 1, header = T, check.names = F)
  obs <- read.delim(paste0(thread,"IrriGW/init_config/Input/Time_series/discharge_obs_24.txt"), skip = 4, header = T, check.names = F)
  noirri <- read.table(paste0(thread,'NoIrri/thread1_best/Output/River_Flow.out'), quote="\"", comment.char="",skip = 1, header = T, check.names = F)
  prec <- read.csv(paste0(thread,"NoIrri/thread1_best/Input/Time_series/rain_daily.dat"), sep="", skip = 2, header = T, check.names = F )
  
  obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
  obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))
  
  obs <-  obs[obs$Date < "2015-01-01",]
  prec <- prec[1:nrow(obs),]
  
  df <- data.frame("Date" = obs$Date, "irri"=irri[,3], "Noirri"=noirri[,3],
                   "Prec" = prec[,match(15,colnames(prec))], "Obs" = obs[,match(15,colnames(obs))])
  png(file ="Uru_irriNoirri.png", bg = "white", width = 2480, height = 1748, res = 300)
  
  par( mai = c(1, 1, 1, 1))
  barplot(df$Prec, ylim = rev(c(0,200)), yaxt = "n", col = rep( "dodgerblue", length(df$Prec)), border = "dodgerblue")
  mtext("Precipitation [mm]", 4, line = 3, cex = 0.8)
  axis(4, cex.axis = 0.8)
  par(new = TRUE)
  plot(df$Obs~df$Date, type = "l", ylim = c(0,2300), cex.axis = 0.8, ylab = expression("Q [m" ^3*"/s]"),
       xlab = "Year", cex.lab = 0.8, cex.axis = 0.8)
  lines(noirri[,3]~obs$Date, type = 'l', col = 'red')
  lines(irri[,3]~obs$Date, type = "l", col = "green")
  abline(h=c(500,1000,1500,2000), lty = 2, col = "grey")
  abline(v= years, lty = 2, col = "grey")
  legend("topright", legend= c("observed", "modeled (no irrigation)","modeled (irrigation)", "precipitation"),col = c("black","red", "green", "dodgerblue"), lty=1, cex=0.8)
  dev.off()



