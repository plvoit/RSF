
# Script for reading in discharge data (fluvio data) and sediment from ANA Hidroweb
# and collect multiple txt-files into one, aggregated by date

# Copyright (C) 2019 José Miguel Delgado, Anne Müller 

# Data structure: Folder with different files of VAZAO for each station

#### check lower section for understanding the code. Because I don't know tidyr. Pretty awesome how it's done!

rm(list = ls())

library(sf)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)


## take fluvio files
setwd("~/Workspace/RioSaoFrancisco/Data/Runoff-data/ANA_AbflussSed/ANA-Qobs")
files <- list.files(pattern=".txt")

## read data using loop and convert
DF <- NULL
for (f in files) {
  dat <- read.table(f, header=TRUE,sep=";",skip=16,dec=",") %>%
    as_tibble() %>%                         #convert to dataframe
    filter(MediaDiaria==1) %>%              #start with column named "MediaDiaria"
    rename(location=`X..EstacaoCodigo`) %>%
    select(-ends_with("Status"),Data) %>%   #select all columns ending with "Status" and column "Data"(=date)
    gather(dom,value,Vazao01:Vazao31) %>%   #Gather takes multiple columns and collapses into key-value pairs
    mutate(date=ymd(as.character(Data))+as.numeric(substr(dom,6,7))-1,value=round(value,1)) %>%   #mutate adds new variables and preserves existing
    select(date,value) %>%
    group_by(date) %>%                      #group data by "date" column
    slice(1) %>%
    ungroup
  
  #DF <- rbind(DF, dat)
  dat=data.frame(dat)
  DF=data.frame(DF)
  colnames(dat)[2] = paste("A",substr(f,7,nchar(f)-4),sep = "_") #rename columns with file name plus "A" for ANA

  if (ncol(DF) == 0) {
    DF=dat 
  } else { 
    DF<- merge(DF,dat,by="date", all=TRUE)
  }
}

## Start with ERA5 starting date, kick out the rest
DF <- DF[DF$date >= "1981-01-01",]

#write.csv(DF,file="~/Workspace/RioSaoFrancisco/Data/Q_ANA.txt")

# #summary(DF)
# 

# ### Save all the stations as interactive dygraph plot
# library(dygraphs)   # for nice interactive plot
# library(xts)        # dygraphs works with xts-format for timeseries
# library(htmlwidgets) #to save widgets
# 
# 
# for(i in 2:ncol(DF)){
#   Vis <- as.xts(DF[,i],DF[,1])
#   graph <-  dygraph(Vis, main= colnames(DF)[i])
#   saveWidget(graph,paste(colnames(DF)[i],".html", sep=""))
# }
# 

# ### step by step to understand the tidyR syntax. 
# ### this is the equivalent to the loop step by step for one file
# 
# dat <- read.table(files[1], header=TRUE,sep=";",skip=16,dec=",")
# dat <- as_tibble(dat)
# dat <- filter(dat,MediaDiaria==1)
# dat <- rename(dat, location=`X..EstacaoCodigo`)
# dat <- select(dat,-ends_with("Status"),Data)  ### -ends_with("Status"):kicks out all the column names that end with "Status", I think Data could be left out and has no effect
# dat <- gather(dat,dom,value,Vazao01:Vazao31) ### the days are in each row. this stacks it, so they're in column and tagged
# dat <- mutate(dat,date=ymd(as.character(Data))+as.numeric(substr(dom,6,7))-1,value=round(value,1)) # creates date column
# dat <- select(dat,date,value)  #select just the two columns date & value
# dat <- group_by(dat,date) # these last three steps somehow order the rows by date
# dat <- slice(dat,1)
# dat <- ungroup(dat)

#-------------------------------------------------------------------
# after visual inspection of the timeseries now the manual correction
# further description in excel file "ANA - zu korrigierende Stationen" 
#-------------------------------------------------------------------

#delete columns that contain no data after 01.01.1981
outcols <- c("A_40300000","A_40811100","A_40817000","A_40821998","A_41200000","A_41340005","A_42145500","A_43430000","A_44760000","A_49160000")
DF[,outcols] <- NULL

#40080000
DF[DF$date >= "2010-01-01","A_40080000"] <-  NA

# the rownames need to be reset to acces the right rows
rownames(DF) <- NULL

#45840000
DF[12905:12906,"A_45840000"] <-  NA
 
#49705000
DF[7459:7465,"A_49705000"] <-  NA
DF[11798:11801,"A_49705000"] <-  NA
DF[11813:11814,"A_49705000"] <-  NA

#test <- DF[,c("date","A_49705000")]

write.csv(DF,file="~/Workspace/RioSaoFrancisco/Data/Q_ANA_corrected.txt")




