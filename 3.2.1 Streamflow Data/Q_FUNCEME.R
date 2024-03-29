## script to format and read the discharge files from FUNCEME

# Copyright (C) 2020 Paul Voit

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
    # dummy <- scan(files[i], what = "character", n = 6)
    # dummy <- dummy[c(1:3,5,6)]
    # dummy <- paste(dummy, collapse = ' ' )
    #Q_file$Comment <-dummy
    Q_file <- Q_file[Q_file$Date >= "1981-01-01",]
    colnames(Q_file)[2] <- paste("F",substr(files_short[i],1,nchar(files_short[i])-4),sep = "_")
    Q_files_list[[i]] <- Q_file
    
  }
  else{
    Q_file <- read.delim(files[i],header=FALSE, skip = 5, na.strings = -9999)
    Q_file$Date <- paste(Q_file$V1, Q_file$V2, Q_file$V3, sep = "-")
    Q_file$Date <- as.POSIXct(Q_file$Date, tz= "UTC", format = "%Y-%m-%d" )
    Q_file <- Q_file[,c(ncol(Q_file),6)]
    colnames(Q_file)[2] <- paste("F",substr(files_short[i],1,nchar(files_short[i])-4),sep = "_")
    
    ## Get Gauge ID information from first line of file
    # dummy <- scan(files[i], what = "charachter", n = 6)
    # dummy <- dummy[c(1:3,5,6)]
    # dummy <- paste(dummy, collapse = ' ' )
    # Q_file$Comment <- dummy
    Q_file <- Q_file[Q_file$Date >= "1981-01-01",]
    Q_files_list[[i]] <- Q_file  
  }
}

##put all the files in one dataframe
Q_DF <- Reduce(function(x, y) merge(x, y, all=T, by="Date"), Q_files_list, accumulate=F)


#write.csv(Q_DF,file="~/Workspace/RioSaoFrancisco/Data/Q_timeseries_FUNCEME.txt")

# ### Save interactive plots
# 
# ### Save all the files as interactive dygraph plot (Raw_Data)
# library(dygraphs)   # for nice interactive plot
# library(xts)        # dygraphs works with xts-format for timeseries
# library(htmlwidgets)
# 
# 
# for(i in 1:length(Q_files_list)){
#   Vis <- as.xts(Q_files_list[[i]][,2],Q_files_list[[i]][,1])
#   graph <-  dygraph(Vis, main= Q_files_list[[i]][1,3])
#   saveWidget(graph,paste(files_short[i],".html", sep=""))
# }


#----------------------------------------------------------------------------------------
#After visual inspection these timeseries will be manually corrected: 49705000,40040000,45590000,46415000,46455000,46550000,46610000,46650000,46675000
# further info in Excel-file FUNCEME - zu korrigierende Stationen
#--------------------------------------------------------------------------------------------

#49705000
Q_DF[11770:11772,"F_49705000"] <- NA
Q_DF[11813:11814,"F_49705000"] <- NA

#40040000
Q_DF[11716:11719,"F_40040000"] <- NA

##45590000
Q_DF[12486,"F_45590000"] <- (Q_DF[12485,"F_45590000"] + Q_DF[12487,"F_45590000"])/2

##46415000 not clear where the shift in the timeseries is, if possible don't use this timeseries

##46455000
Q_DF[Q_DF$Date >= "2011-08-01","F_46455000"] <- NA

##46550000  ### shift by 70 in march 2005
Q_DF[Q_DF$Date >= "2005-03-01" & Q_DF$Date < "2005-04-01","F_46550000"] <- Q_DF[Q_DF$Date >= "2005-03-01" & Q_DF$Date < "2005-04-01","F_46550000"] + 70
Q_DF[10705:10706,"F_46550000"] <-  NA

##46610000
Q_DF[10353:10357,"F_46610000"] <- NA

#46105000
Q_DF[which(Q_DF[,"F_46105000"] == 0),"F_46105000"] <- NA

#46650000
Q_DF[10440:10445,"F_46650000"] <- NA

#46675000
Q_DF[9772:9792,"F_46675000"] <- NA

#49705000
Q_DF[which(Q_DF[,"F_49705000"] == 0),"F_49705000"] <- NA

### write file corrected dataframe
write.csv(Q_DF,file="~/Workspace/RioSaoFrancisco/Data/Q_FUNCEME_corrected.txt", row.names = FALSE)


# for (i in 1:length(Q_files_list)){
#   plot(Q_files_list[[i]][,2]~Q_files_list[[i]][,1], xlab = "Date", ylab = "Q", main = Q_files_list[[i]][1,3],  type = "l")
# }
# 
# 
which(all_merged[,"F_49705000"] == 0)


