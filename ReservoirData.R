## this script reads the ONS reservoir data and checks the naturalized flows
## Finally from the data of FUNCEME, ANA and ONS a dataframe with coordinates
## with stations used for the calibration of WASA is created


#### SUBBASIN ID has to be added for WASA, see temporary script "add_subbasinID"

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)
library(readxl)

files <-  dir("Data/ANA reservoir Data/Workdir", full.names =  TRUE)
files_short <-  dir("Data/ANA reservoir Data/Workdir")

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


TRES_F <-  Q_files_list[[2]]

TRES_F156 <-  Q_files_list[[1]]


TRES_Res <- read_excel("Data/ANA reservoir Data/Historico_TRES_MARIAS.xlsx" )
TRES_Res[11]  <- as.POSIXct(TRES_Res[[11]])
TRES_Res[6] <- apply(TRES_Res[6],1, function(x) gsub(",",".",x))
TRES_Res[6] <- as.numeric(TRES_Res[[6]])


plot(TRES_F$F_41020002~TRES_F$Date, type = "l")
plot(TRES_F156$F_156~TRES_F156$Date, type = "l", col = "red")
lines(TRES_Res$`Vazão Vertida (m³/s)`~TRES_Res$`Data da Medição`, col = "blue", type ="l")

#  Interactive graph displaying CRNS, Reference Soil Moisture and Precipitation, Code by Berry
# from young Hydro script

library(xts)
library(dygraphs)

Res_graph <- merge(TRES_F,TRES_F156, by = "Date", all = TRUE)
Res_graph <- merge(Res_graph, TRES_Res, by.x = "Date", by.y = "Data da Medição", all =TRUE)
Res_graph <- Res_graph[,c(1,2,3,7,8,11)]


graph <- as.xts(Res_graph, Res_graph$Date )
# initiate the dygraph
graph <- dygraph(graph)%>%
  # define the first axis
  dyAxis(colnames(Res_graph)[4], name = "y", label = "m^3/s") %>%
  
  # plot the data
  dySeries(colnames(Res_graph)[4] ,axis = 'y')%>%
  dySeries(colnames(Res_graph)[5] ,axis = 'y')%>%
  dySeries(colnames(Res_graph)[6] ,axis = 'y')%>%
  dySeries(colnames(Res_graph)[7] ,axis = 'y')%>%
  dyOptions(colors = c("black","red","green", "blue")) %>%
  dyLegend("onmouseover") %>%
  dyRangeSelector()

graph

## create graphs of all the in and outflow of the reservoirs
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(PaulsPack)
library(readxl)
library(xts)
library(dygraphs)
library(htmlwidgets)


files <-  dir("Data/ONS reservoir Data/Files", full.names =  TRUE, pattern = ".csv")
files_short <-  dir("Data/ONS reservoir Data/Files", pattern = ".csv")

file_list <- list()

for ( i in 1:length(files)){
  dummy <- read.csv2(files[i])
  dummy$Data.da.Medição <- as.POSIXct(dummy$Data.da.Medição, format = c("%d.%m.%Y"))
  file_list[[i]] <- dummy
}

# 
# for(i in 1:length(file_list)){
#   dummy <- file_list[[i]]
#   dummy[which(dummy[,"Vazão.Vertida..m³.s."] == 0),"Vazão.Vertida..m³.s."] <- NA
#   dummy[which(dummy[,"Vazão.Turbinada..m³.s."] == 0),"Vazão.Turbinada..m³.s."] <- NA
#   
#   # create DF for graph
#   Res_graph <-  dummy[,c(4,5,6,7,11)]
#   graph <- as.xts(Res_graph, Res_graph$Data.da.Medição )
#   # initiate the dygraph
#   graph <- dygraph(graph, main = substr(files_short[i],11,nchar(files_short[i])-4) )%>%
#     # define the first axis
#     dyAxis(colnames(Res_graph)[1], name = "y", label = "m^3/s") %>%
#     
#     # plot the data
#     #dySeries(colnames(Res_graph)[1] ,axis = 'y')%>% #already defined by dyAxis
#     dySeries(colnames(Res_graph)[2] ,axis = 'y')%>%
#     dySeries(colnames(Res_graph)[3] ,axis = 'y')%>%
#     dySeries(colnames(Res_graph)[4] ,axis = 'y')%>%
#     dyOptions(colors = c("black","red","green", "blue", "red")) %>% # no idea why you have to give 5 colours for for timeseries but it works
#     dyLegend("onmouseover") %>%
#     dyRangeSelector()
#   #save as html widget
#   saveWidget(graph,paste(substr(files_short[i],11,nchar(files_short[i])-4),".html", sep=""))
# }


for (i in 1:length(file_list)){
  dummy <-  file_list[[i]]
  plot(dummy[[4]] ~dummy$Data.da.Medição, type ="l", ylab = "m^3/s", 
       main = substr(files_short[i],11,nchar(files_short[i])-4), xlab = "Year", ylim= c(0,max(dummy[,c(4,5,6,7)], na.rm = TRUE)))
  lines(dummy[[5]] ~ dummy$Data.da.Medição, type ="l", col = "red")
  lines(dummy[[6]] ~ dummy$Data.da.Medição, type ="l", col = "blue")
  lines(dummy[[7]] ~ dummy$Data.da.Medição, type ="l", col = "green")
  legend("topright", legend = c("Afluência","Defluência","Vazao Vertida","Vazao Turbinada"), col = c("black","red","blue","green"), lty=1, cex=0.8)
}


#plot Vazao natural


for (i in 1:length(file_list)){
  dummy <-  file_list[[i]]
  plot(dummy[[8]]~dummy$Data.da.Medição, type ="l", ylab = "m^3/s",
       main = substr(files_short[i],11,nchar(files_short[i])-4))
  legend("topright", legend = c("Vazao Natural"), col = c("black"), lty=1, cex=0.8)
}


# Vazao Natural dygraph
for(i in 1:length(file_list)){
    dummy <- file_list[[i]]
    Vis <- as.xts(dummy["Vazão.Natural..m³.s."] ,dummy$Data.da.Medição)
    graph <-  dygraph(Vis, main = paste("V. Natural",substr(files_short[i],11,nchar(files_short[i])-4), sep = " "), ylab = "m^3/s")
    saveWidget(graph,paste(substr(files_short[i],11,nchar(files_short[i])-4),".html", sep=""))
  }


# #are 3 of the identical ? Moxoto,Xingo,Paulo Alfonso
# PauloAlfonso <- as.data.frame(file_list[[3]])
# Moxoto <- file_list[[2]]
# Xingo <- file_list[[3]]
# LuizGonzaga <- file_list[[1]] 
# 
# 
# plot(PauloAlfonso[[8]]~PauloAlfonso[[11]], type ="l" )
# lines(LuizGonzaga[[8]]~LuizGonzaga[[11]], type ="l", col = "green")
# lines(Moxoto[[8]]~Moxoto[[11]], type ="l", col = "red")
# lines(Xingo[[8]]~Xingo[[11]], type = "l", col = "blue")

# Combined set of 17 stations for calibration including FUNCEME,ANA and naturalized flow from ONS

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

stations_cal <- c("F_40050000","F_40100000","TRES MARIAS","F_42210000","F_45298000","F_46035000","F_46360000",
                  "SOBRADINHO","F_48590000","XINGO","F_49705000","F_40850000","F_41990000","A_42980000",
                  "F_43880000","F_45260000","F_46902000")



ANA_coord <- read.csv("~/Workspace/RioSaoFrancisco/GIS/PegelCSV/ANA_coord.csv")
FUNCEME_coord <- read.csv("~/Workspace/RioSaoFrancisco/GIS/PegelCSV/FUNCEME_coord.csv")
reservoir_coord <- read.csv("~/Workspace/RioSaoFrancisco/GIS/PegelCSV/Reservoir_coord.csv")

colnames(ANA_coord)[3] <-  c("ID")
colnames(FUNCEME_coord)[3] <-  "ID"
colnames(reservoir_coord)[3] <- "ID"

ANA_coord <- ANA_coord[,-4]
FUNCEME_coord <- FUNCEME_coord[,-4]
reservoir_coord <- reservoir_coord[,-4]

#add identifiers for ANA and FUNCEME
for (i in 1:nrow(ANA_coord)){
  ANA_coord[i,3] <- paste("A_",ANA_coord[i,3], sep = "")
}



for (i in 1:nrow(FUNCEME_coord)){
  FUNCEME_coord[i,3] <- paste("F_",FUNCEME_coord[i,3], sep = "")
}

all_coord <- rbind(ANA_coord,FUNCEME_coord,reservoir_coord)

station_cal <- all_coord[all_coord$ID %in% stations_cal,]

write.csv(station_cal,"Data/stations_cal.txt", row.names = FALSE)
