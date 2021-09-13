####Gütemaß improvements
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe")

## Gütenmaße für Z2IrriCal
# subbasins <- c(1,2,3,4,11,12,13,14,15)
 ns_irri <- c(0.58, 0.6, 0.68, 0.76, 0.53, 0.36, 0.38, -6.32, -6.73)
 rmse_lf_irri <- c(553, 531, 454, 226, 68, 106, 63, 95)
 ns_noirri <- c(0.56, 0.57, 0.62, 0.7, 0.47, 0.34, 0.39, -4.4, -10.7)
 rmse_lf_norirri <- c(603.7, 579, 562.5, 236.3, 68.3, 120.8, 78 )
 rmse_q_irri <- c(907.2, 894.7, 831.2, 378.3, 221.7, 379.3, 173, 84.4, 196.3 )
 rmse_q_noirri <- c(899.2, 901.9, 848.6, 415.9, 235.9 , 382.7, 168.3, 91.2, 242)
 d <- ns_irri - ns_noirri
#png(file = "ObjectiveFunctions_irriNoirri.png", bg = "white", width = 1500 , height =  2480, res = 300)
par(mfrow = c(3, 1), mai = c(0.35,0.5,0.2,0.2), oma = c(2, 2, .5, .5), mgp = c(2, .6, 0))


# for barplot RMSE_monthly
Z2Irri <- read.table("Z2IrriCal/Irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("rmse_monthly", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriCal/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("rmse_monthly", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2IrriCal/NoIrri/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2C) <- c("obj_fun","val")
Z2C <- Z2C[grep("rmse_monthly", Z2C[,1]),]
Z2C$obj_fun <- as.factor(Z2C$obj_fun)
Z2C$Subgroup <- as.factor("C")

AllZ2 <- rbind(Z2Irri,Z2NoIrri, Z2C)

data_base <- reshape(AllZ2,                        # Modify data for Base R barplot
                     idvar = "Subgroup",
                     timevar = "obj_fun",
                     direction = "wide")
row.names(data_base) <- data_base$Subgroup
data_base <- data_base[ , 2:ncol(data_base)]
data_base <- data_base[,c(9,7,6,3,1,2,4,5,8)]
colnames(data_base) <- c("1", "2", "3", "4", "11", "12", "13", "14", "15")
data_base <- as.matrix(data_base)


##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1,3),]

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#D40E92", "#4AB29D",'#000099'), ylim = c(0,20000))
abline(h=0)
title(ylab=expression("RMSE"["monthly"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)

### LowFlow
# for barplot RMSE_monthly
Z2Irri <- read.table("Z2IrriCal/Irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("loflo", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriCal/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("loflo", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2IrriCal/NoIrri/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2C) <- c("obj_fun","val")
Z2C <- Z2C[grep("loflo", Z2C[,1]),]
Z2C$obj_fun <- as.factor(Z2C$obj_fun)
Z2C$Subgroup <- as.factor("C")

AllZ2 <- rbind(Z2Irri,Z2NoIrri, Z2C)

data_base <- reshape(AllZ2,                        # Modify data for Base R barplot
                     idvar = "Subgroup",
                     timevar = "obj_fun",
                     direction = "wide")
row.names(data_base) <- data_base$Subgroup
data_base <- data_base[ , 2:ncol(data_base)]
data_base <- data_base[,c(9,7,6,3,1,2,4,5,8)]
colnames(data_base) <- c("1", "2", "3", "4", "11", "12", "13", "14", "15")
data_base <- as.matrix(data_base)


##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1,3),]

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#D40E92", "#4AB29D",'#000099') , ylim = c(0,600))
abline(h=0)
title(ylab=expression("RMSE"["lowflow"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)
title(xlab="Gauge number", line=1.4, cex.lab=1.2)


###NSE

Z2Irri <- read.table("Z2IrriCal/Irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("ns_co", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriCal/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("ns_co", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2IrriCal/NoIrri/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2C) <- c("obj_fun","val")
Z2C <- Z2C[grep("ns_co", Z2C[,1]),]
Z2C$obj_fun <- as.factor(Z2C$obj_fun)
Z2C$Subgroup <- as.factor("C")

AllZ2 <- rbind(Z2Irri,Z2NoIrri, Z2C)

# for barplot NSE
data_base <- reshape(AllZ2,                        # Modify data for Base R barplot
                     idvar = "Subgroup",
                     timevar = "obj_fun",
                     direction = "wide")
row.names(data_base) <- data_base$Subgroup
data_base <- data_base[ , 2:ncol(data_base)]
data_base <- data_base[,c(9,7,6,3,1,2,4,5,8)]
colnames(data_base) <- c("1", "2", "3", "4", "11", "12", "13", "14", "15")
data_base <- as.matrix(data_base)


##take out the two negative subs (Carinhana and )
data_base <- data_base[,1:7]
data_base <- data_base[c(2,1,3),]

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, ylab= "NSE", col = c("#D40E92", "#4AB29D",'#000099'), ylim = c(0,0.8), cex.lab = 1.2)
abline(h=0)
legend("topright", legend= c("Set A", "Set B", "Set C"),col = c("#D40E92", "#4AB29D",'#000099'), pch = 15)
title(xlab="Gauge number", line=1.4, cex.lab=1.2)
#dev.off()

##compare low flows Set B and Set C
lf_b <- AllZ2[AllZ2[3] == "Irri",]
lf_c <- AllZ2[AllZ2[3] == "C",]

d <- lf_c$val - lf_b$val
d_per <- d/lf_c$val * 100
d_per

