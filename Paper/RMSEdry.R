####Gütemaß improvements
#### Z2
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs")

irri <- read.table("Z2Irri/irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
obs <- read.delim("Z2Irri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)

obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))

obs <-  obs[obs$Date < "2015-01-01",]

dry_season <- c(120:220)
subs <- c('13','73','78','15','16','90','58','45','96')
RMSE_dry <- matrix(0,nrow = length(subs), ncol=15)

for(i in 1:length(subs)){
  for(j in 1:15){
    dry_season <- c(120:220)+365*(j-1)
    rmse_drySeason= sqrt(mean((obs[dry_season,match(subs[i],colnames(obs))]-irri[dry_season,match(subs[i],colnames(irri))])^2, na.rm=TRUE))
    RMSE_dry[i,j] <- round(rmse_drySeason,1)
  }
}

years <- (2000:2014)

for(i in 1:length(subs)){
  plot(RMSE_dry[i,]~years, type = "l", main = subs[i])
}

### plot just dry seasons
irri <- read.table("Z2Irri/irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
obs <- read.delim("Z2Irri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)

obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))

obs <-  obs[obs$Date < "2015-01-01",]


dry_season <- c(200:300)
dry_seasons <- dry_season
for(i in 1:13) dry_seasons <- c(dry_seasons,dry_season+ i*365) #get the indexes for the whole modelled period (14 years

irri[-dry_seasons,c(2:ncol(irri))]  <- NA
obs[-dry_seasons,c(5:ncol(obs)-1)] <-  NA

for(i in 1:length(subs)){
  plot(irri[,match(subs[i],colnames(irri))]~obs[,ncol(obs)], main = subs[i], type = "l", col = "green")
}


# where are the dryseasons? 
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs")

period <- c(120:305)

prec <- read.csv("Z2Irri/init_config/Input/Time_series/rain_daily.dat", skip = 2, sep="", check.names = F)
prec <- prec[1:5479,]

obs <- read.delim("Z2Irri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))
obs <-  obs[obs$Date < "2015-01-01",]

## use dygraphs to find the periods by hand

#remove(list = ls())

library(dygraphs)   # for nice interactive plot
library(xts)        # dygraphs works with xts-format for timeseries
library(htmlwidgets) #to save widgets

subs <- c('13','73','78','15','16','90','58','45','96')

 for(i in 1:length(subs)){
   Vis <- as.xts(obs[,match("subs[i]",colnames(obs))],obs$Date)
   graph <-  dygraph(Vis, main= subs[i])
   saveWidget(graph,paste(subs[i],"_obs.html", sep=""))
 }
  

Vis <- as.xts(obs[,match("90",colnames(obs))],obs$Date)
graph <-  dygraph(Vis, main= "78")
saveWidget(graph,paste("90","_obs.html", sep=""))


### simulation days ofdry seasons were derived visually/manually:
sub13 <- c(92:306,397:682,742:1036,1217:1401,1582:1783,1947:2131,2312:2596,2677:2861,3044:3226,
           3407:3575,3759:3941,4122:4304,4487:4686,4856:5077,5142:5416)
sub15 <- c(92:306,464:640,822:1036,1202:1401,1582:1804,1926:2131,2322:2471,2677:2880,3042:3243,
           3417:3591,3757:3956,4137:4303,4472:4686,4867:5074,5232:5411)
sub58 <- c(106:344,457:671,836:1036,1217:1431,1596:1796,1947:2131,2318:2486,2677:2889,3044:3237,
           3417:3585,3727:3956,4137:4345,4487:4690,4867:5081,5232:5385)
sub73 <- c(122:306,457:671,836:1074,1217:1416,1613:1793,1947:2131,2312:2496,2677:2881,3042:3234,
           3407:3584,3772:3956,4137:4335,4516:4686,4867:4990,5232:5416)
sub78 <- c(101:306,447:671,822:1036,1187:1431,1582:1796,1927:2131,2282:2496,2616:2891,3042:3235,
           3407:3577,3772:3956,4121:4342:4502:4686,4867:5074,5222:5445)
sub90 <- c(106:306,457:771,836:1036,1217:1431,1596:1796,1931:2131,2312:2484,2661:2891,3042:3239,
           3421:3580,3772:3956,4137:4342,4486:4686,4867:5081,5232:5410)
sub96 <- c(122:306,457:671,852:1036,1217:1431,1596:1796,1947:2137,2312:2496,2661:2891,3042:3241,
           3421:3584,3761:3956,4137:4351,4502:4691,4867:5081,5246:5416)
