rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)


ANA <-read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/Q_ANA_corrected.txt")
ANA <- colnames(ANA)
ANA <- ANA[-1]

for (i in 1:length(ANA)){
  ANA[i] <- substr(ANA[i],3,nchar(ANA[i]))
}



FUNCEME <- read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/Q_FUNCEME_corrected.txt")
FUNCEME <- colnames(FUNCEME)
FUNCEME <- FUNCEME[-1]

for (i in 1:length(FUNCEME)){
  FUNCEME[i] <- substr(FUNCEME[i],3,nchar(FUNCEME[i]))
}

# which stations appear in both datasets?
twice <- ANA[ANA %in% FUNCEME]

ANA <-read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/Q_ANA_corrected.txt")
FUNCEME <- read.csv("~/Workspace/RioSaoFrancisco/Data/Processed/Q_FUNCEME_corrected.txt")

# is the data the same?
# 
# for (i in 1:length(twice)){
# plot(ANA[,paste("A_",twice[i],sep = "")], type = "l", main = twice[i])
# lines(FUNCEME[,paste("F_",twice[i],sep = "")], type = "l", col = "red")
# }

# yes, it's the same data, ANA timeseries are often a bit longer

# create vector that contains the ones that will get kicked out
outcols <- c("F_40080000","F_40100000","F_40150000","A_40185000","A_40300001","F_40330000","A_40400000",
             "A_40680000","F_40740000","F_40800001","A_40810350","F_40823500","F_42100000","F_42395000",
             "F-42540000","F_42600000","F_42750000","F_42980000","F_43200000","A_43300000","A_43429998",
             "A_43670000","A_44540000","F_45131000","A_45170001","A_45260000","A_45298000","F_45480000",
             "A_45590000","F_45740001","F_45770000","F_45840000","F_45910001","F_45960001","A_47480000",
             "A_49705000")

all_merged <- merge(ANA,FUNCEME,by.x = "date", by.y = "Date", all = TRUE)

all_merged[,outcols] <- NULL

