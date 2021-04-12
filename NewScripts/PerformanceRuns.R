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
