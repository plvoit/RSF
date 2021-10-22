###### Pot Irri NoIrri Cal
#plot and save
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs")

irri <- read.table("Z2Irri/irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
obs <- read.delim("Z2Irri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
noirri <- read.csv("Z2NoIrriNew/SetA/Output/River_Flow.out", sep="", skip=1, header = T, check.names = F)

prec <- read.csv("Z2NoIrriNew/SetA/Input/Time_series/rain_daily.dat", sep="", skip = 2, header = T, check.names = F )

obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))

obs <-  obs[obs$Date < "2015-01-01",]

prec <- prec[1:nrow(obs),]
####
plot(irri$`58`~obs$Date, type = "l")
lines(obs$`96`~obs$Date, type = "l", col = "green")

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
years <- as.POSIXct(c('2000-01-01', '2001-01-01','2002-01-01','2003-01-01', '2004-01-01', '2005-01-01', '2006-01-01', '2007-01-01', '2008-01-01',
                      '2009-01-01','2010-01-01','2011-01-01','2012-01-01','2012-01-01','2014-01-01', '2015-01-01'))


  png(file = "Multiplot_irriNoirri.png", bg = "white", width = 2480, height = 1748, res = 300)
  par(mfrow = c(2, 1), mai = c(0.3,0.5,0.5, 0.1), mar = numeric(4), oma = c(4, 4, .5, .5), 
      mgp = c(2, .6, 0))


  df1 <- data.frame("Date" = obs$Date, "irri"=irri[,match(subs[1],colnames(irri))], "Noirri"=noirri[,match(subs[1],colnames(noirri))],
                   "Prec" = prec[,match(subs[1],colnames(prec))], "Obs" = obs[,match(subs[1],colnames(obs))])
  
  plot(df1$Obs~df$Date, type = "l", ylim = c(0,max(df1$irri, df1$Noirri, df1$Obs, na.rm = T)), cex.axis = 0.8, ylab = expression("discharge [m" ^3*"/s]"),
       cex.lab = 0.8, cex.axis = 0.8, xaxt = 'n', xlab ="")
  lines(df1$Noirri~df1$Date, type = 'l', col = 'red')
  lines(df1$irri~df1$Date, type = "l", col = "green")
  abline(h=seq(2000,10000,2000), lty = 2, col = "grey")
  abline(v= years, lty = 2, col = "grey")
  #legend("topright", legend= c("observed", "modeled (no irrigation)","modeled (irrigation)"),col = c("black","red", "green"), lty=1, cex=0.8)
  
  # second plot
  df2 <- data.frame("Date" = obs$Date, "irri"=irri[,match(subs[2],colnames(irri))], "Noirri"=noirri[,match(subs[2],colnames(noirri))],
                   "Prec" = prec[,match(subs[2],colnames(prec))], "Obs" = obs[,match(subs[2],colnames(obs))])
  plot(df2$Obs~df$Date, type = "l", ylim = c(0,max(df2$irri, df2$Noirri, df2$Obs, na.rm = T)), cex.axis = 0.8, ylab = expression("discharge [m" ^3*"/s]"),
       xlab = "Year", cex.lab = 0.8, cex.axis = 0.8)
  axis.Date(1, at = c('2000-01-01', '2002-01-01'),
            format= "%Y")
  lines(df2$Noirri~df2$Date, type = 'l', col = 'red')
  lines(df2$irri~df2$Date, type = "l", col = "green")
  abline(h=seq(2000,10000,2000), lty = 2, col = "grey")
  abline(v= years, lty = 2, col = "grey")
  legend("topright", legend= c("observed", "modelled (without irrigation)","modelled (with irrigation)"),col = c("black","red", "green"), lty=1, cex=0.8)
  mtext("Year", side = 1, outer = TRUE, line = 2.2)
  mtext(expression("Q [m" ^3*"/s]"), side = 2, outer = TRUE, line = 2.2)
  
  dev.off()
  
  
### Zoom for LowFlows Sub 58
  
  sub58 <- list(106:344,457:671,836:1036,1217:1431,1596:1796,1947:2131,2318:2486,2677:2889,3044:3237,
                3417:3585,3727:3956,4137:4345,4487:4690,4867:5081,5232:5385)
  
  ticklabels = c('Jan',"Feb","Mar",'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  year = c(2000:2015)
  df1 <- data.frame("Date" = obs$Date, "irri"=irri[,match(subs[1],colnames(irri))], "Noirri"=noirri[,match(subs[1],colnames(noirri))],
                    "Prec" = prec[,match(subs[1],colnames(prec))], "Obs" = obs[,match(subs[1],colnames(obs))])
  
  i=1
  png(file ="Sub58_LowFlowZoom.png", bg = "white", width = 2000, height = 2480, res = 300)
  par(mfrow = c(8, 2), mai=c(0,0.5,0.3,0), oma = c(2, 2, .5, .5))
  
  for (i in 1:14){
    ticks = as.POSIXct(c(paste0('01.01.',year[i]), paste0('01.02.',year[i]), paste0('01.03.',year[i]), paste0('01.04.',year[i]), paste0('01.05.',year[i]),
                         paste0('01.06.',year[i]), paste0('01.07.',year[i]), paste0('01.08.',year[i]),paste0('01.09.',year[i]), paste0('01.10.',year[i]),
                         paste0('01.11.',year[i]), paste0('01.12.',year[i])), format=c("%d.%m.%Y"))
    
    season= sub58[[i]]
    plot(df1$Obs[season]~df1$Date[season], type = "l", cex.axis = 1, ylab = "",ylim = c(min(df1$irri[season]-300),max(df1$irri[season]+250)),
         xlab = year[i], cex.lab = 0.8, xaxt='n')
    axis(1,ticks, ticklabels)
    title(ylab = expression("Q [m" ^3*"/s]"), line = 2, cex.lab = 1 )    #move ylabel closer to axis
    title(main = as.character(year[i]), line =-1, cex.main = 1)
    lines(df1$Noirr[season]~df1$Date[season], type = 'l', col = 'red')
    lines(df1$irri[season]~df1$Date[season], type = "l", col = "green")
  }
  dev.off()

ylim_list <- list(c(500,3500))
  
  plot(df1$Obs~df$Date, type = "l", ylim = c(0,max(df1$irri, df1$Noirri, df1$Obs, na.rm = T)), cex.axis = 0.8, ylab = expression("discharge [m" ^3*"/s]"),
       cex.lab = 0.8, cex.axis = 0.8, xaxt = 'n', xlab ="")
  lines(df1$Noirri~df1$Date, type = 'l', col = 'red')
  lines(df1$irri~df1$Date, type = "l", col = "green")
  abline(h=seq(2000,10000,2000), lty = 2, col = "grey")
  abline(v= years, lty = 2, col = "grey")

#### RAINFALL RUNOFF for Rio Urucuia

  ###### lPot Irri NoIrri Cal
  #plot and save
  rm(list = ls())
  setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs/10000runs")
  
  years <- as.POSIXct(c('2000-01-01', '2001-01-01','2002-01-01','2003-01-01', '2004-01-01', '2005-01-01', '2006-01-01', '2007-01-01', '2008-01-01',
                        '2009-01-01','2010-01-01','2011-01-01','2012-01-01','2012-01-01','2014-01-01', '2015-01-01'))
  
  irri <- read.table("UruIrri/irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
  obs <- read.delim("UruIrri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
  noirri <- read.table("UruIrri/noirri/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
  prec <- read.csv("UruIrri/irri_best/Input/Time_series/rain_daily.dat", sep="", skip = 2, header = T, check.names = F )
  
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
  legend("topright", legend= c("observed", "modelled (without irrigation)","modelled (with irrigation)", "precipitation"),col = c("black","red", "green", "dodgerblue"), lty=1, cex=0.8)
  dev.off()

#####Zoom for lowflos in dry season
  sub15 <- list(92:306,464:640,822:1036,1202:1401,1582:1804,1926:2131,2322:2471,2677:2880,3042:3243,
                3417:3591,3757:3956,4137:4303,4472:4686,4867:5074,5232:5411)
  
  ticklabels = c('Jan',"Feb","Mar",'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  
  year = c(2000:2014)
  png(file ="Uru_LowFlowZoom.png", bg = "white", width = 2000, height = 2480, res = 300)
  par(mfrow = c(8, 2), mai=c(0,0.5,0.3,0), oma = c(2, 2, .5, .5))
  for (i in 1:15){
    ticks = as.POSIXct(c(paste0('01.01.',year[i]), paste0('01.02.',year[i]), paste0('01.03.',year[i]), paste0('01.04.',year[i]), paste0('01.05.',year[i]),
                         paste0('01.06.',year[i]), paste0('01.07.',year[i]), paste0('01.08.',year[i]),paste0('01.09.',year[i]), paste0('01.10.',year[i]),
                         paste0('01.11.',year[i]), paste0('01.12.',year[i])), format=c("%d.%m.%Y"))
    
    season= sub15[[i]]
    plot(df$Obs[season]~df$Date[season], type = "l", ylim = c(0,250), cex.axis = 1, ylab = "",
         xlab = year[i], cex.lab = 0.8, xaxt='n')
    axis(1,ticks, ticklabels)
    title(ylab = expression("Q [m" ^3*"/s]"), line = 2, cex.lab = 1 )    #move ylabel closer to axis
    title(main = as.character(year[i]), line =-1, cex.main = 1)
    lines(noirri[season,3]~obs$Date[season], type = 'l', col = 'red')
    lines(irri[season,3]~obs$Date[season], type = "l", col = "green")
  }
  dev.off()

