rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/WASA-SED/0/")

thread <- "Zone3cal"

# load clearer names from CheckWASAoutput.R for later labelling the plots
# load GaugeNumber-SubbasID file
SubbasID_GaugeNumber <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)

# subbas_id = c("10","11","12") #Zone 1
# subbas_id=c(78,73,15,16,90,58,96,45) #Zone2
subbas_id = c("1","2","3") #Zone3

obs <- read.delim(paste(thread,"/Input/Time_series/discharge_obs_24.txt", sep = "") , header= T, skip = 4, check.names  = F)
mod <- read.table(paste(thread,"/Output/River_Flow.out", sep = ""), quote="\"", comment.char="", skip = 1, header = T, check.names = F)

obs <- obs[,c(1:4, match(subbas_id,colnames(obs)))]
mod <- mod[,c(1,2,match(subbas_id,colnames(mod)))]


## Plot modeled and observed river discharge
for (i in subbas_id){

plot(obs[[i]], type = "l", main = SubbasID_GaugeNumber[match(i,SubbasID_GaugeNumber$Subbas_ID),3], xlab = "days", ylab = "m3/s")
lines(mod[[i]], type = "l", col = "red")
legend("topright", legend= c("observed", "modeled"),col = c("black","red"), lty=1, cex=0.8)
}


obs$Date <- as.POSIXct(paste(obs$YYYY,obs$MM,obs$DD, sep = "-"), format = c("%Y-%m-%d"))
obs$MonthYear <- format(obs$Date, "%Y-%m")
mod$Date <- obs$Date
mod$MonthYear <- obs$MonthYear

obs_monthly_sum <- aggregate(obs[,subbas_id], by = list(obs$MonthYear), sum)
mod_monthly_sum <- aggregate(mod[,subbas_id], by = list(mod$MonthYear), sum)


# plot monthly sums
for (i in subbas_id){
  
  plot(obs_monthly_sum[[i]], type = "l", main = SubbasID_GaugeNumber[match(i,SubbasID_GaugeNumber$Subbas_ID),3], xlab = "months", ylab = "m3")
  lines(mod_monthly_sum[[i]], type = "l", col = "red")
  legend("topright", legend= c("observed", "modeled"),col = c("black","red"), lty=1, cex=0.8)
}


## Residuals
residual_monthly <- mod_monthly_sum[,match(subbas_id, colnames(mod_monthly_sum))] - obs_monthly_sum[,match(subbas_id,colnames(obs_monthly_sum))]
residual_monthly$Date <- as.POSIXct(paste(obs_monthly_sum$Group.1,"-01",sep = ""), format = c("%Y-%m-%d"))
residual_monthly <- residual_monthly[,c(ncol(residual_monthly),1:length(subbas_id))]

## monthly error in percent

error_percent <- round(residual_monthly[,match(subbas_id, colnames(residual_monthly))] / obs_monthly_sum[,match(subbas_id,colnames(obs_monthly_sum))] * 100, 2)
colnames(error_percent) <- SubbasID_GaugeNumber[match(colnames(error_percent),SubbasID_GaugeNumber$Subbas_ID),3]
error_percent$Date <- residual_monthly$Date
error_percent <- error_percent[,c(ncol(error_percent),1:length(subbas_id))]

summary(error_percent[,c(2:(1+length(subbas_id)))])

#mean monthly percentual error

mean_month_err <- aggregate(error_percent, by = list(format(error_percent$Date, "%m" )), mean, na.rm = TRUE)
mean_month_err <- mean_month_err[,c(1,3:ncol(mean_month_err))]
names(mean_month_err)[1]  <- "Month"


for ( i in 2:ncol(mean_month_err)){
  plot(mean_month_err[[i]]~mean_month_err$Month, type = "l", xlab = "Month", ylab = "mean monthly error [%]", main = paste("Mean monthly error: ",colnames(mean_month_err)[i], sep = ""))
}


plot(mean_month_err[[10]]~mean_month_err$Month, type = "l")

