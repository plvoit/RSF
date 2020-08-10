# This script extracts the Outflow from the reservoirs and stores them in a csv
# Dividing the RSF basin in 4 parts requires this data as input for parts 2,3,4


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

files <-  dir("Data/ONS reservoir Data/Workdir", full.names =  TRUE)
files_short <-  dir("Data/ONS reservoir Data/Workdir")

## read all files and format. There seem to be two different formats among the original files


for (i in 1:length(files)){
  dummy <- read.csv(files[i], sep=";", stringsAsFactors = FALSE)
  res_name <- dummy[1,2]
  dummy <- dummy[,c(5,11)]
  dummy[,2] <- gsub(",",".",dummy[,2])
  names(dummy) <- c("Defluencia m3/s","Date")
  write.csv(dummy,file = paste(res_name,"_Defluencia.txt", sep = ""), row.names = FALSE)
}



