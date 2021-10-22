####Gütemaß improvements
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs")

#dry seasons
sub13 <- list(92:306,397:682,742:1036,1217:1401,1582:1783,1947:2131,2312:2596,2677:2861,3044:3226,
           3407:3575,3759:3941,4122:4304,4487:4686,4856:5077,5142:5416)
sub15 <- list(92:306,464:640,822:1036,1202:1401,1582:1804,1926:2131,2322:2471,2677:2880,3042:3243,
           3417:3591,3757:3956,4137:4303,4472:4686,4867:5074,5232:5411)
sub58 <- list(106:344,457:671,836:1036,1217:1431,1596:1796,1947:2131,2318:2486,2677:2889,3044:3237,
           3417:3585,3727:3956,4137:4345,4487:4690,4867:5081,5232:5385)
sub73 <- list(122:306,457:671,836:1074,1217:1416,1613:1793,1947:2131,2312:2496,2677:2881,3042:3234,
           3407:3584,3772:3956,4137:4335,4516:4686,4867:4990,5232:5416)
sub78 <- list(101:306,447:671,822:1036,1187:1431,1582:1796,1927:2131,2282:2496,2616:2891,3042:3235,
           3407:3577,3772:3956,4121:4342,4502:4686,4867:5074,5222:5445)
sub90 <- list(106:306,457:771,836:1036,1217:1431,1596:1796,1931:2131,2312:2484,2661:2891,3042:3239,
           3421:3580,3772:3956,4137:4342,4486:4686,4867:5081,5232:5410)
sub96 <- list(122:306,457:671,852:1036,1217:1431,1596:1796,1947:2137,2312:2496,2661:2891,3042:3241,
           3421:3584,3761:3956,4137:4351,4502:4691,4867:5081,5246:5416)

obs <- read.delim("Z2Irri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))
obs <-  obs[obs$Date < "2015-01-01",]

irri  <- read.table("Z2Irri/irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
noirri <- read.table("Z2NoIrriNew/thread1_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)

subs <- c('13','73','78','15','90','58','96')

yearly_dry_rmse_irri <- c()
yearly_dry_rmse_noirri <- c()

years <- c(2000:2014)
difference_list <-  list()

for(j in 1:length(subs)){
  for(i in 1:15){
    yearly_dry_rmse_irri[i] <-  sqrt(mean((obs[get(paste0("sub",subs[j]))[[i]],match(subs[j],colnames(obs))]-irri[get(paste0("sub",subs[j]))[[i]],match(subs[j],colnames(irri))])^2, na.rm=TRUE))
    yearly_dry_rmse_noirri[i] <-  sqrt(mean((obs[get(paste0("sub",subs[j]))[[i]],match(subs[j],colnames(obs))]-noirri[get(paste0("sub",subs[j]))[[i]],match(subs[j],colnames(noirri))])^2, na.rm=TRUE))
  }
  diff_rmse <- yearly_dry_rmse_noirri - yearly_dry_rmse_irri
  difference_list[[j]] <- diff_rmse
  plot(diff_rmse~years, main = subs[j])
  abline(lm(diff_rmse~years), lty=2)
}


### plot sub58 and 96 in one plot
png(file = "diffRMSE.png", bg = "white", width = 2200 , height =  1800, res = 300)
plot(difference_list[[6]]~years, col="blue", pch = 18, cex = 2, ylab="", cex.lab=1.2, xlab = "Year"  )
abline(lm(difference_list[[6]]~years), lty=3, col="blue")
points(difference_list[[7]]~years, col="red", pch = 18, cex = 2)
abline(lm(difference_list[[7]]~years), lty=3, col="red")
title(ylab = expression(paste("RMSE"["L"]* " Set A - RMSE"["L"]* " Set B")), line = 2.5, cex.lab=1.2)
legend("bottomright", legend=c("Sub 58", "Sub 96"), col=c("blue", "red"), pch = 18, cex = 1.5)
dev.off()

