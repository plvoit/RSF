rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(readxl)
Runs <- read_excel("PerformanceRuns.xlsx", col_names = F, skip = 1)

qtotal <- Runs[c(1:15),]
qlow <- Runs[c(19:33),]
names(qlow) <- names(qtotal)

qtotal <- as.data.frame(t(qtotal)) 
rownames(qtotal) <- NULL

#  first  row as column names
names(qtotal) <- as.matrix(qtotal[1, ])
qtotal <- qtotal[-1, ]
qtotal[] <- lapply(qtotal, function(x) type.convert(as.character(x)))

# order the factors
qtotal$`WASA run` <- factor(qtotal$`WASA run`, levels=c("calibrated normal", "vegetation calibrated" , "added lake module" , "added irrigation module"))
qtotal$RunNr <- c(1,2,3,4)

plot(qtotal$`11`~qtotal$RunNr, type = "l", ylim= c(0,max(qtotal[,2:ncol(qtotal)], na.rm = T)))

for (i in 3:(ncol(qtotal)-1)){
  lines(qtotal[,i]~qtotal$RunNr,type = "l")
  points(qtotal[,i]~qtotal$RunNr)
}

# or with matplot
matplot(qtotal[,c(2:(ncol(qtotal)-1))], type = c("o"),pch=1,col = 1:4) #plot


#normalize
qtotal_norm <- qtotal

for(i in 2:(ncol(qtotal_norm)-1)){
 qtotal_norm[,i] <-  sapply(qtotal_norm[,i], function(x) x <- round(x/max(qtotal_norm[,i], na.rm = T),2))
}

matplot(qtotal_norm[,c(2:(ncol(qtotal_norm)-1))], type = c("o"),pch=1,col = 2:ncol(qtotal)-1, xaxt = "n") #plot
axis(side=1, at= qtotal_norm$`WASA run`, labels= levels(qtotal_norm$`WASA run`), cex.axis = 0.8)


##New
### compare WASA results with different modules
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
### New Approach
## Irri Results
Z1I <- read.table("ResultsCalibration/Mod/AllZ1/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, na.string = "NaN")
names(Z1I) <- c("obj_fun","val")
## take this parameterset because it was better than the one achieved when calibrated with irrigation module
Z2I <- read.delim("ResultsCalibration/Mod/AllSensZ2/Parms1/SensRateResults.txt", header=F)
Z2I <- Z2I[2:nrow(Z2I),c(1,11)]#
names(Z2I) <- c("obj_fun","val")
Z3I <- read.table("ResultsCalibration/Mod/AllZ3/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, na.string = "NaN")
names(Z3I) <- c("obj_fun","val")

AllI <- rbind(Z1I,Z2I,Z3I)
AllI$val <- as.numeric(AllI$val)

#Lake results
L1 <- read.table("ResultsCalibration/Mod/LakesZ1/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(Z1I) <- c("obj_fun","val")
L2 <- read.table("ResultsCalibration/Mod/LakesZ2/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(Z2I) <- c("obj_fun","val")
L3 <- read.table("ResultsCalibration/Mod/LakesZ3/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(Z3I) <- c("obj_fun","val")

AllL <- rbind(L1,L2,L3)

#modifified vegetation results

M1 <- read.table("ResultsCalibration/Mod/CalVegZ1/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M1) <- c("obj_fun","val")
M2 <- read.table("ResultsCalibration/Mod/CalVegZ2/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M2) <- c("obj_fun","val")
M3 <- read.table("ResultsCalibration/Mod/CalVegZ3/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M3) <- c("obj_fun","val")

MAll <- rbind(M1,M2,M3)

#Original Parameters

O1 <- read.table("ResultsCalibration/NoIrriZones/Zone1new/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M1) <- c("obj_fun","val")
O2 <- read.table("ResultsCalibration/NoIrriZones/Zone2new/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M2) <- c("obj_fun","val")
O3 <- read.table("ResultsCalibration/NoIrriZones/Zone3new/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3)
names(M3) <- c("obj_fun","val")

OAll <- rbind(O1,O2,O3)

#join all of the to one dataframe

All <- cbind(OAll,MAll$val,AllL$V2,AllI$val)
names(All) <- c("obj_fun","Original","mod_veg","Lakes","Irri")
#All[is.na(All)] <- 0
All$Irri <- as.numeric(All$Irri)

#take out unnecessary rows
All <- All[-(grep("sed_bias",All[,1])),]
All <- All[-(grep("penalty",All[,1])),]
All <- All[-(grep("rmse_qbas",All[,1])),]

#for EGU
All <- All[,c(1,4,5)]
All$Diff <- All[,2] - All[,3]
All$Improvement <- "test"

for( i in 1:nrow(All)){
  if(All[i,4] > 0) All[i,5] <- "YES"
  else(All[i,5] <- "NO")
}

loflo <- All[grep("loflo", All[,1]),]
table(loflo$Improvement)

hiflo <- All[grep("hiflo", All[,1]),]
table(hiflo$Improvement)

corr <- All[grep("cor_total", All[,1]),]
table(corr$Improvement)

qtotal <-  All[grep("qtotal", All[,1]),]
table(qtotal$Improvement)

ns_co <- All[grep("ns_co", All[,1]),]
table(ns_co$Improvement)

for(i in 1:nrow(All)){
  dummy <- t(All[i,c(2,3)])
  plot(dummy, type = "l", main = All[i,1])
}  


AllNS <- All[grep("ns_co", All[,1]),]
AllNS <- AllNS[,c(1,4)]
