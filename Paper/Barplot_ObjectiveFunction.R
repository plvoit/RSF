####Gütemaß improvements
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs")

png(file = "ObjectiveFunctions_irriNoirri.png", bg = "white", width = 1500 , height =  2480, res = 300)
par(mfrow = c(2, 1), mai = c(1,1,0.2,0.2), oma = c(2, 2, .5, .5), mgp = c(2, .6, 0))


# for barplot RMSE_monthly
Z2Irri <- read.table("Z2Irri/irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("rmse_monthly", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriNew/SetA/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("rmse_monthly", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2Irri/noirri/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2C) <- c("obj_fun","val")
Z2C <- Z2C[grep("rmse_monthly", Z2C[,1]),]
Z2C$obj_fun <- as.factor(Z2C$obj_fun)
Z2C$Subgroup <- as.factor("C")

Z2D <- read.table("Z2NoIrriNew/SetD/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2D) <- c("obj_fun","val")
Z2D <- Z2D[grep("rmse_monthly", Z2D[,1]),]
Z2D$obj_fun <- as.factor(Z2D$obj_fun)
Z2D$Subgroup <- as.factor("D")

AllZ2 <- rbind(Z2Irri,Z2NoIrri)

data_base <- reshape(AllZ2,                        # Modify data for Base R barplot
                     idvar = "Subgroup",
                     timevar = "obj_fun",
                     direction = "wide")
row.names(data_base) <- data_base$Subgroup
data_base <- data_base[ , 2:ncol(data_base)]
data_base <- data_base[,-c(5,8)]
data_base <- data_base[,c(7,6,5,3,1,2,4)]
colnames(data_base) <- c("96", "58", "90", "78", "13", "73", "15")
data_base <- as.matrix(data_base)


##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1),]

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#4AB29D","#D40E92"), ylim = c(0,20000))
abline(h=0)
title(ylab=expression("RMSE"["m"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)

percent <- ((data_base[1,] -data_base[2,])/data_base[1,]) * 100

############################
## RMSE dry season
Z2Irri <- read.table("Z2Irri/irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("dry", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriNew/SetA/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("dry", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2Irri/noirri/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2C) <- c("obj_fun","val")
Z2C <- Z2C[grep("dry", Z2C[,1]),]
Z2C$obj_fun <- as.factor(Z2C$obj_fun)
Z2C$Subgroup <- as.factor("C")

Z2D <- read.table("Z2NoIrriNew/SetD/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2D) <- c("obj_fun","val")
Z2D <- Z2D[grep("dry", Z2D[,1]),]
Z2D$obj_fun <- as.factor(Z2D$obj_fun)
Z2D$Subgroup <- as.factor("D")


#####Refill dataframe AllZ2 with newly calculated RMSE dry season values
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
           3407:3577,3772:3956,4121:4342,4502:4686,4867:5074,5222:5445)
sub90 <- c(106:306,457:771,836:1036,1217:1431,1596:1796,1931:2131,2312:2484,2661:2891,3042:3239,
           3421:3580,3772:3956,4137:4342,4486:4686,4867:5081,5232:5410)
sub96 <- c(122:306,457:671,852:1036,1217:1431,1596:1796,1947:2137,2312:2496,2661:2891,3042:3241,
           3421:3584,3761:3956,4137:4351,4502:4691,4867:5081,5246:5416)

obs <- read.delim("Z2Irri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))
obs <-  obs[obs$Date < "2015-01-01",]

irri  <- read.table("Z2Irri/irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
setC <- read.table("Z2Irri/noirri/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
noirri <- read.table("Z2NoIrriNew/SetA/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
setD<- read.table("Z2NoIrriNew/SetD/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
 
subs <- c('13','73','78','15','90','58','96')

for (i in 1:length(subs)){
  Z2Irri[grepl(subs[i],Z2Irri[,1]),2] <- sqrt(mean((obs[get(paste0("sub",subs[i])),match(subs[i],colnames(obs))]-irri[get(paste0("sub",subs[i])),match(subs[i],colnames(irri))])^2, na.rm=TRUE))
}

for (i in 1:length(subs)){
  Z2NoIrri[grepl(subs[i],Z2NoIrri[,1]),2] <- sqrt(mean((obs[get(paste0("sub",subs[i])),match(subs[i],colnames(obs))]-noirri[get(paste0("sub",subs[i])),match(subs[i],colnames(noirri))])^2, na.rm=TRUE))
}

for (i in 1:length(subs)){
  Z2C[grepl(subs[i],Z2C[,1]),2] <- sqrt(mean((obs[get(paste0("sub",subs[i])),match(subs[i],colnames(obs))]-setC[get(paste0("sub",subs[i])),match(subs[i],colnames(setC))])^2, na.rm=TRUE))
}

for (i in 1:length(subs)){
  Z2D[grepl(subs[i],Z2D[,1]),2] <- sqrt(mean((obs[get(paste0("sub",subs[i])),match(subs[i],colnames(obs))]-setD[get(paste0("sub",subs[i])),match(subs[i],colnames(setD))])^2, na.rm=TRUE))
}

AllZ2 <- rbind(Z2Irri,Z2NoIrri)

data_base <- reshape(AllZ2,                        # Modify data for Base R barplot
                     idvar = "Subgroup",
                     timevar = "obj_fun",
                     direction = "wide")
row.names(data_base) <- data_base$Subgroup
data_base <- data_base[ , 2:ncol(data_base)]
data_base <- data_base[,-c(5,8)]
data_base <- data_base[,c(7,6,5,3,1,2,4)]
colnames(data_base) <- c("96", "58", "90", "78", "13", "73", "15")
data_base <- as.matrix(data_base)

##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1),]

percent <- ((data_base[1,] -data_base[2,])/data_base[1,]) * 100 

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#4AB29D","#D40E92") , ylim = c(0,500))
abline(h=0)
title(ylab=expression("RMSE"["L"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)
title(xlab="Subbasin", line=1.8, cex.lab=1.2)
legend("topright", legend= c("Set A", "Set B"),col = c("#4AB29D","#D40E92"), pch = 15)

dev.off()

# ###NSE
#
# Z2Irri <- read.table("Z2Irri/irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
# names(Z2Irri) <- c("obj_fun","val")
# Z2Irri <- Z2Irri[grep("ns_co", Z2Irri[,1]),] 
# Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
# Z2Irri$Subgroup <- as.factor("Irri")
# 
# Z2NoIrri <- read.table("Z2NoIrriNew/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
# names(Z2NoIrri) <- c("obj_fun","val")
# Z2NoIrri <- Z2NoIrri[grep("ns_co", Z2NoIrri[,1]),]
# Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
# Z2NoIrri$Subgroup <- as.factor("NoIrri")
# 
# Z2C <- read.table("Z2Irri/noirri/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
# names(Z2C) <- c("obj_fun","val")
# Z2C <- Z2C[grep("ns_co", Z2C[,1]),]
# Z2C$obj_fun <- as.factor(Z2C$obj_fun)
# Z2C$Subgroup <- as.factor("C")
# 
# #
#  AllZ2 <- rbind(Z2Irri,Z2NoIrri, Z2C)
# 
# # # for barplot NSE
#  data_base <- reshape(AllZ2,                        # Modify data for Base R barplot
#                       idvar = "Subgroup",
#                       timevar = "obj_fun",
#                       direction = "wide")
#  row.names(data_base) <- data_base$Subgroup
#  data_base <- data_base[ , 2:ncol(data_base)]
#  data_base <- data_base[,c(9,7,6,3,1,2,4,5,8)]
#  colnames(data_base) <- c("1", "2", "3", "4", "11", "12", "13", "14", "15")
#  data_base <- as.matrix(data_base)
# 
#  
# # ##take out the two negative subs (Carinhana and )
#  data_base <- data_base[,1:7]
#  data_base <- data_base[c(2,1,3),]
# # 
# ## take out 2 last columns, change colors
# barplot(height = data_base, beside = TRUE, ylab= "NSE", col = c("#D40E92", "#4AB29D",'#000099'), ylim = c(0,0.8), cex.lab = 1.2)
# abline(h=0)
# legend("topright", legend= c("Set A", "Set B", "Set C"),col = c("#D40E92", "#4AB29D",'#000099'), pch = 15)
# title(xlab="Gauge number", line=1.4, cex.lab=1.2)
# #dev.off()
# 
# ##compare low flows Set B and Set C
# lf_b <- AllZ2[AllZ2[3] == "Irri",]
# lf_c <- AllZ2[AllZ2[3] == "C",]
# 
# d <- lf_c$val - lf_b$val
# d_per <- d/lf_c$val * 100
# d_per



### For Urucuia Subbasin
## RMSE dry season
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs/10000runs")

UruIrri <- read.table("UruIrri/irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(UruIrri) <- c("obj_fun","val")
UruIrri <- UruIrri[grep("loflo", UruIrri[,1]),] 
UruIrri$obj_fun <- as.factor(UruIrri$obj_fun)
UruIrri$Subgroup <- as.factor("Irri")

UruNoIrri <- read.table("test/SetA/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(UruNoIrri) <- c("obj_fun","val")
UruNoIrri <- UruNoIrri[grep("loflo", UruNoIrri[,1]),]
UruNoIrri$obj_fun <- as.factor(UruNoIrri$obj_fun)
UruNoIrri$Subgroup <- as.factor("NoIrri")

UruC <- read.table("UruIrri/noirri/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(UruC) <- c("obj_fun","val")
UruC <- UruC[grep("loflo", UruC[,1]),]
UruC$obj_fun <- as.factor(UruC$obj_fun)
UruC$Subgroup <- as.factor("C")

UruD <- read.table("test/SetD/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(UruD) <- c("obj_fun","val")
UruD <- UruD[grep("loflo", UruD[,1]),]
UruD$obj_fun <- as.factor(UruD$obj_fun)
UruD$Subgroup <- as.factor("D")

#####Refill dataframe AllUru with newly calculated RMSE dry season values
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
           3407:3577,3772:3956,4121:4342,4502:4686,4867:5074,5222:5445)
sub90 <- c(106:306,457:771,836:1036,1217:1431,1596:1796,1931:2131,2312:2484,2661:2891,3042:3239,
           3421:3580,3772:3956,4137:4342,4486:4686,4867:5081,5232:5410)
sub96 <- c(122:306,457:671,852:1036,1217:1431,1596:1796,1947:2137,2312:2496,2661:2891,3042:3241,
           3421:3584,3761:3956,4137:4351,4502:4691,4867:5081,5246:5416)

obs <- read.delim("UruIrri/init_config/Input/Time_series/discharge_obs_24.txt", skip = 4, header = T, check.names = F)
obs$Date <-  paste(obs$YYYY,obs$MM,obs$DD, sep = "-")
obs$Date <- as.POSIXct(obs$Date, format = c("%Y-%m-%d"))
obs <-  obs[obs$Date < "2015-01-01",]

irri  <- read.table("UruIrri/irri_best/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
setC <- read.table("UruIrri/noirri/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
noirri <- read.table("test/SetA/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)
setD <- read.table("test/SetD/Output/River_Flow.out", quote="\"", comment.char="",skip = 1, header = T, check.names = F)

subs <- c('15')

for (i in 1:length(subs)){
  UruIrri[grepl(subs[i],UruIrri[,1]),2] <- sqrt(mean((obs[get(paste0("sub",subs[i])),match(subs[i],colnames(obs))]-irri[get(paste0("sub",subs[i])),match(subs[i],colnames(irri))])^2, na.rm=TRUE))
}

for (i in 1:length(subs)){
  UruNoIrri[grepl(subs[i],UruNoIrri[,1]),2] <- sqrt(mean((obs[get(paste0("sub",subs[i])),match(subs[i],colnames(obs))]-noirri[get(paste0("sub",subs[i])),match(subs[i],colnames(noirri))])^2, na.rm=TRUE))
}

for (i in 1:length(subs)){
  UruC[grepl(subs[i],UruC[,1]),2] <- sqrt(mean((obs[get(paste0("sub",subs[i])),match(subs[i],colnames(obs))]-setC[get(paste0("sub",subs[i])),match(subs[i],colnames(setC))])^2, na.rm=TRUE))
}

for (i in 1:length(subs)){
  UruD[grepl(subs[i],UruD[,1]),2] <- sqrt(mean((obs[get(paste0("sub",subs[i])),match(subs[i],colnames(obs))]-setD[get(paste0("sub",subs[i])),match(subs[i],colnames(setD))])^2, na.rm=TRUE))
}

AllUru <- rbind(UruIrri,UruNoIrri, UruC, UruD)

data_base <- reshape(AllUru,                        # Modify data for Base R barplot
                     idvar = "Subgroup",
                     timevar = "obj_fun",
                     direction = "wide")
row.names(data_base) <- data_base$Subgroup
data_base <- as.matrix(data_base)

##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1,3),]

percent <- ((data_base[1,] -data_base[2,])/data_base[1,]) * 100 

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#D40E92", "#4AB29D",'#000099') , ylim = c(0,500))
abline(h=0)
title(ylab=expression("RMSE"["L"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)
title(xlab="Subbasin", line=1.8, cex.lab=1.2)
legend("topright", legend= c("Set A", "Set B", "Set C"),col = c("#D40E92", "#4AB29D",'#000099'), pch = 15)

dev.off()

