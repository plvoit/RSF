rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

## read and format the files
gauge <- read.delim("~/Workspace/RioSaoFrancisco/Data/Runoff-data/FUNCEME-GRDC_Abfluss_UFZ/3650745.txt",
                   header=FALSE, skip = 5)
gauge$V1 <- as.character(gauge$V1)
gauge$Date <- substr(gauge$V1,1,16)
gauge$Discharge <- substr(gauge$V1,20,27)
gauge <- gauge[,c(2,3)]
gauge$Date <- as.POSIXct(gauge$Date, tz= "UTC", format = "%Y %m %d" )
gauge[gauge$Discharge == "-9999.00",2] <-  NA
gauge$Discharge <- as.numeric(gauge$Discharge)

## Get Gauge ID information from first line of file
dummy <- scan("~/Workspace/RioSaoFrancisco/Data/Runoff-data/FUNCEME-GRDC_Abfluss_UFZ/3650745.txt", what = "charachter", n = 6)
dummy <- paste(dummy, collapse = ' ' )
gauge$Comment <-dummy


## ERA5 starts 01.01.1981, so everything before isn't needed

gauge <- gauge[gauge$Date >= "1981-01-01",]
