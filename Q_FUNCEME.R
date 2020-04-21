## script to format and read the discharge files from FUNCEME

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)

files <- dir("~/Workspace/RioSaoFrancisco/Data/Runoff-data/FUNCEME-GRDC_Abfluss_UFZ/", full.names = TRUE)

files_short <- dir("Data/Runoff-data/FUNCEME-GRDC_Abfluss_UFZ")


## read all files and format. There seem to be two different formats among the original files
Q_files_list <- list()

for (i in 1:length(files)){
  if (nchar(files_short[i]) == 11){
    Q_file <- read.delim(files[i],header=FALSE, skip = 5)
    Q_file$V1 <- as.character(Q_file$V1)
    Q_file$Date <- substr(Q_file$V1,1,16)
    Q_file$Q <- substr(Q_file$V1,20,27)
    Q_file <- Q_file[,c(2,3)]
    Q_file$Date <- as.POSIXct(Q_file$Date, tz= "UTC", format = "%Y %m %d" )
    Q_file[Q_file$Q == "-9999.00",2] <-  NA
    Q_file$Q <- as.numeric(Q_file$Q)
    
    ## Get Q_file ID information from first line of file
    dummy <- scan(files[i], what = "character", n = 6)
    dummy <- dummy[c(1:3,5,6)]
    dummy <- paste(dummy, collapse = ' ' )
    Q_file$Comment <-dummy
    Q_file <- Q_file[Q_file$Date >= "1981-01-01",]
    Q_files_list[[i]] <- Q_file
  }
  else{
    Q_file <- read.delim(files[i],header=FALSE, skip = 5, na.strings = -9999)
    Q_file$Date <- paste(Q_file$V1, Q_file$V2, Q_file$V3, sep = "-")
    Q_file$Date <- as.POSIXct(Q_file$Date, tz= "UTC", format = "%Y-%m-%d" )
    Q_file <- Q_file[,c(ncol(Q_file),6)]
    colnames(Q_file)[2] <- "Q"
    
    ## Get Gauge ID information from first line of file
    dummy <- scan(files[i], what = "charachter", n = 6)
    dummy <- dummy[c(1:3,5,6)]
    dummy <- paste(dummy, collapse = ' ' )
    Q_file$Comment <- dummy
    Q_file <- Q_file[Q_file$Date >= "1981-01-01",]
    Q_files_list[[i]] <- Q_file  
  }
  }

for (i in 1:length(Q_files_list)){
  plot(Q_files_list[[i]][,2]~Q_files_list[[i]][,1], xlab = "Date", ylab = "Q", main = Q_files_list[[i]][1,3],  type = "l")
}


### Save interactive plots

### Save all the files as interactive dygraph plot (Raw_Data)
library(dygraphs)   # for nice interactive plot
library(xts)        # dygraphs works with xts-format for timeseries
library(htmlwidgets)


for(i in 1:length(Q_files_list)){
  Vis <- as.xts(Q_files_list[[i]][,2],Q_files_list[[i]][,1])
  graph <-  dygraph(Vis, main= Q_files_list[[i]][1,3])
  saveWidget(graph,paste(files_short[i],".html", sep=""))
}


