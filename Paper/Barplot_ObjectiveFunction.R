####Gütemaß improvements
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/Paper/NewExe/6000runs")

#png(file = "ObjectiveFunctions_irriNoirri.png", bg = "white", width = 1500 , height =  2480, res = 300)
#par(mfrow = c(3, 1), mai = c(0.35,0.5,0.2,0.2), oma = c(2, 2, .5, .5), mgp = c(2, .6, 0))


# for barplot RMSE_monthly
Z2Irri <- read.table("Z2Irri/irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("rmse_monthly", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriNew/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("rmse_monthly", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2Irri/thread1/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
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
colnames(data_base) <- c("96", "58", "90", "78", "13", "73", "15", "16", "45")
data_base <- as.matrix(data_base)


##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1,3),]

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#D40E92", "#4AB29D",'#000099'), ylim = c(0,20000))
abline(h=0)
title(ylab=expression("RMSE"["monthly"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)

############################
### q10
Z2Irri <- read.table("Z2Irri/irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("q10", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriNew/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("q10", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2Irri/thread1/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2C) <- c("obj_fun","val")
Z2C <- Z2C[grep("q10", Z2C[,1]),]
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
colnames(data_base) <- c("96", "58", "90", "78", "13", "73", "15", "16", "45")
data_base <- as.matrix(data_base)

##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1,3),]

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#D40E92", "#4AB29D",'#000099') , ylim = c(0,1000))
abline(h=0)
title(ylab=expression("RMSE"["q10"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)
title(xlab="Gauge number", line=1.4, cex.lab=1.2)

#######################
# q25
Z2Irri <- read.table("Z2Irri/irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("q25", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriNew/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("q25", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2Irri/thread1/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2C) <- c("obj_fun","val")
Z2C <- Z2C[grep("q25", Z2C[,1]),]
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
colnames(data_base) <- c("96", "58", "90", "78", "13", "73", "15", "16", "45")
data_base <- as.matrix(data_base)

##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1,3),]

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#D40E92", "#4AB29D",'#000099') , ylim = c(0,1000))
abline(h=0)
title(ylab=expression("RMSE"["q25"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)
title(xlab="Gauge number", line=1.4, cex.lab=1.2)

############################
## RMSE dry
Z2Irri <- read.table("Z2Irri/irri_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2Irri) <- c("obj_fun","val")
Z2Irri <- Z2Irri[grep("dry", Z2Irri[,1]),] 
Z2Irri$obj_fun <- as.factor(Z2Irri$obj_fun)
Z2Irri$Subgroup <- as.factor("Irri")

Z2NoIrri <- read.table("Z2NoIrriNew/thread1_best/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2NoIrri) <- c("obj_fun","val")
Z2NoIrri <- Z2NoIrri[grep("dry", Z2NoIrri[,1]),]
Z2NoIrri$obj_fun <- as.factor(Z2NoIrri$obj_fun)
Z2NoIrri$Subgroup <- as.factor("NoIrri")

Z2C <- read.table("Z2Irri/thread1/curr_obj_fun_val_day.txt", quote="\"", comment.char="", skip = 3, stringsAsFactors = F)
names(Z2C) <- c("obj_fun","val")
Z2C <- Z2C[grep("dry", Z2C[,1]),]
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
colnames(data_base) <- c("96", "58", "90", "78", "13", "73", "15", "16", "45")
data_base <- as.matrix(data_base)

##take out the two negative subs (Carinhana and )
#data_base <- data_base[,1:7]
data_base <- data_base[c(2,1,3),]

## take out 2 last columns, change colors
barplot(height = data_base, beside = TRUE, col = c("#D40E92", "#4AB29D",'#000099') , ylim = c(0,500))
abline(h=0)
title(ylab=expression("RMSE"["dryseason"]*" [m" ^3*"/s]"), line=2, cex.lab=1.2)
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

