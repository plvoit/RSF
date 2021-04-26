rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(rgeos)
library(sf)
library(rgdal)
library(sp)

#load all licences, scraped with the script scrapeANA.R
allLicenses <- read.csv("~/Workspace/RioSaoFrancisco/Data/Irrigation/ScrapeANA/LicencesRSFBasin.txt")

#transform to spatial points

licencesShape <- SpatialPointsDataFrame(coords = c(allLicenses[,c("LONGITUDE","LATITUDE")]), proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"),allLicenses)

##load subbasin shape

subbasins <- readOGR("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/GIS/75subbas-neu/Shape/75SubbasWGS84.shp")
subbasins <- subbasins[subbasins$DN != 999, ]

#plot(subbasins)
#plot(licencesShape, add = T)


#IrriSub <- over(subbasins,licencesShape, returnList = T)

#irridat <- data.frame("sub_source" = , "source" = , "sub_receiver" = , "rule" = "seasonal",
#                      "rate" = , "rate2" = , "rate3" = ),
#                      "rate4" = , loss_factor = 0.8)

# filter out the irrigation abstractions
licencesShape[is.na(licencesShape$FINALIDADE_PRINCIPAL),10] <- "Irrigação"
licencesShape <- licencesShape[licencesShape$FINALIDADE_PRINCIPAL == "Irrigação",]

## group sources to categories for irrigation module

sources <- as.character(unique(licencesShape$CORPO_HIDRICO))

river <- c("Rio", "Riacho", "Ribeirão", "Córrego", "SEM NOME", "Vereda", "Ipueira"  ) # and NA's
river_names <- c()
for (i in 1:length(river)){
  river_names <- c(river_names,sources[grepl(river[i], sources)]) 
}


acudes <-  c("Açude", "Barragem", "Lagoa", "Baixa", "Baixo")
acude_names <- c()
for (i in 1:length(acudes)){
  acude_names <- c(acude_names,sources[grepl(acudes[i], sources)])
}

reservoir_names <- sources[grepl("UHE", sources)]


# which ones are left? How many
length(sources[!(sources %in% c(river_names,acude_names,reservoir_names))])
## just 2, so I just ignore them...

## add source-column to data frame. Better to do it now once then always in a loop
licencesShape@data$CORPO_HIDRICO <- as.character(licencesShape@data$CORPO_HIDRICO)

for(i in 1:nrow(licencesShape@data)){
  if(licencesShape@data$CORPO_HIDRICO[i] %in% river_names) licencesShape@data$IrriSource[i] <-  "River"
  if(licencesShape@data$CORPO_HIDRICO[i] %in% acude_names) licencesShape@data$IrriSource[i] <-  "Acude"
  if(licencesShape@data$CORPO_HIDRICO[i] %in% reservoir_names) licencesShape@data$IrriSource[i] <-  "Reservoir"
}

river_irri <- data.frame( "Subbasin" = subbasins$DN)
acude_irri <- data.frame( "Subbasin" = subbasins$DN)

# get monthly irrigation sums for each subbasin
for( i in 1:nrow(subbasins@data)){
  basin <- subbasins[subbasins$DN == subbasins$DN[i],]
  dummy <- licencesShape[basin,]
  dummy <-  dummy@data  
  
  #####
  #River

  dummy2 <- dummy[dummy$IrriSource == "River",]
  
    dummy2 <- dummy2[,-c(1,2)]
    # calculate the daily volume of water abstracted (m^3/d) (daily Volume * days per month * hours per day)
  
    river_irri[i,2] <- sum((dummy2[,19] * dummy2[,33] * dummy2[,34])/ 31, na.rm = T )
    river_irri[i,3] <- sum((dummy2[,20] * dummy2[,35] * dummy2[,36])/ 28, na.rm = T)
    river_irri[i,4] <- sum((dummy2[,21] * dummy2[,37] * dummy2[,38])/ 31, na.rm = T)
    river_irri[i,5] <- sum((dummy2[,22] * dummy2[,39] * dummy2[,40])/ 30, na.rm = T)
    river_irri[i,6] <- sum((dummy2[,23] * dummy2[,41] * dummy2[,42])/ 31, na.rm = T)
    river_irri[i,7] <- sum((dummy2[,24] * dummy2[,43] * dummy2[,44])/ 30, na.rm = T)
    river_irri[i,8] <- sum((dummy2[,25] * dummy2[,45] * dummy2[,46])/ 31, na.rm = T)
    river_irri[i,9] <- sum((dummy2[,26] * dummy2[,47] * dummy2[,48])/ 31, na.rm = T)
    river_irri[i,10] <- sum((dummy2[,27] * dummy2[,49] * dummy2[,50])/ 30, na.rm = T)
    river_irri[i,11] <- sum((dummy2[,28] * dummy2[,51] * dummy2[,52])/ 31, na.rm = T)
    river_irri[i,12] <- sum((dummy2[,29] * dummy2[,53] * dummy2[,54])/ 30, na.rm = T)
    river_irri[i,13] <- sum((dummy2[,30] * dummy2[,55] * dummy2[,56])/ 31, na.rm = T)
  
  ######
  ## Acudes
  
  dummy2 <- dummy[dummy$IrriSource == "Acude",]
  
    dummy2 <- dummy2[,-c(1,2)]
    # calculate the daily volume of water abstracted (m^3/d) (daily Volume * days per month * hours per day)
    
    acude_irri[i,2] <- sum((dummy2[,19] * dummy2[,33] * dummy2[,34])/ 31, na.rm = T )
    acude_irri[i,3] <- sum((dummy2[,20] * dummy2[,35] * dummy2[,36])/ 28, na.rm = T)
    acude_irri[i,4] <- sum((dummy2[,21] * dummy2[,37] * dummy2[,38])/ 31, na.rm = T)
    acude_irri[i,5] <- sum((dummy2[,22] * dummy2[,39] * dummy2[,40])/ 30, na.rm = T)
    acude_irri[i,6] <- sum((dummy2[,23] * dummy2[,41] * dummy2[,42])/ 31, na.rm = T)
    acude_irri[i,7] <- sum((dummy2[,24] * dummy2[,43] * dummy2[,44])/ 30, na.rm = T)
    acude_irri[i,8] <- sum((dummy2[,25] * dummy2[,45] * dummy2[,46])/ 31, na.rm = T)
    acude_irri[i,9] <- sum((dummy2[,26] * dummy2[,47] * dummy2[,48])/ 31, na.rm = T)
    acude_irri[i,10] <- sum((dummy2[,27] * dummy2[,49] * dummy2[,50])/ 30, na.rm = T)
    acude_irri[i,11] <- sum((dummy2[,28] * dummy2[,51] * dummy2[,52])/ 31, na.rm = T)
    acude_irri[i,12] <- sum((dummy2[,29] * dummy2[,53] * dummy2[,54])/ 30, na.rm = T)
    acude_irri[i,13] <- sum((dummy2[,30] * dummy2[,55] * dummy2[,56])/ 31, na.rm = T)
  
}

# ##########################
# # Seasonality from monthly data
# Day <-  c(31,28,31,30,31,30,31,31,30,31,30,31)
# DF <- data.frame("doy" = seq(1,365,1), "Irri" = 0)
# 
# for ( i in 1:nrow(acude_irri)){
#   
#   MonthlyIrri <- acude_irri[i,2:ncol(acude_irri)]
# 
#   ## create ASCI-Input file for seasonality script
# 
#   DF$Irri <- c(rep(MonthlyIrri[1,1],Day[1]),
#              rep(MonthlyIrri[1,2],Day[2]),
#              rep(MonthlyIrri[1,3],Day[3]),
#              rep(MonthlyIrri[1,4],Day[4]),
#              rep(MonthlyIrri[1,5],Day[5]),
#              rep(MonthlyIrri[1,6],Day[6]),
#              rep(MonthlyIrri[1,7],Day[7]),
#              rep(MonthlyIrri[1,8],Day[8]),
#              rep(MonthlyIrri[1,9],Day[9]),
#              rep(MonthlyIrri[1,10],Day[10]),
#              rep(MonthlyIrri[1,11],Day[11]),
#              rep(MonthlyIrri[1,12],Day[12]))
#   DF$Irri <- round(DF$Irri,0)
# 
# #setwd("RSF/3.3 Irrigation Data")
# write.table(DF,file =paste("Seasonality/Data/Acudes/Irri_season_Sub",river_irri$Subbasin[i],".txt", sep = ""), sep = "\t", row.names = FALSE, quote = FALSE)
# }
#plot(DF$Irri)
#source("seasonality.R",chdir = TRUE)

Day <-  c(31,28,31,30,31,30,31,31,30,31,30,31)
DF <- data.frame("doy" = seq(1,365,1), "Irri" = 0)

i= "107"

  MonthlyIrri <- acude_irri[acude_irri$Subbasin == i,c(2:13) ]
  
  ## create ASCI-Input file for seasonality script
  
  DF$Irri <- c(rep(MonthlyIrri[1,1],Day[1]),
               rep(MonthlyIrri[1,2],Day[2]),
               rep(MonthlyIrri[1,3],Day[3]),
               rep(MonthlyIrri[1,4],Day[4]),
               rep(MonthlyIrri[1,5],Day[5]),
               rep(MonthlyIrri[1,6],Day[6]),
               rep(MonthlyIrri[1,7],Day[7]),
               rep(MonthlyIrri[1,8],Day[8]),
               rep(MonthlyIrri[1,9],Day[9]),
               rep(MonthlyIrri[1,10],Day[10]),
               rep(MonthlyIrri[1,11],Day[11]),
               rep(MonthlyIrri[1,12],Day[12]))
  DF$Irri <- round(DF$Irri,0)

plot(DF$Irri, ylim = c(0,max(DF$Irri)+500) )
rowMeans(acude_irri[acude_irri$Subbasin == i,c(2:13) ])

#Manually add Points for seasonality
library("zoo")
DOY <- c(32,135,210,310)
DF$Approx <- 0
DF$Approx[DOY] <- DF$Irri[DOY]

# calculate Rate at first and last day of year by connecting first DOY and last DOy (linear)
dx <- DOY[1] + 365 - DOY[4]
dy <- DF$Irri[DOY[1]] - DF$Irri[DOY[4]]
slope <- dy/dx
intercept <- DF$Irri[DOY[4]] + slope * (365 - DOY[4])
DF$Approx[c(1,365)] <- intercept

DF[DF$Approx == 0,3] <- NA
DF$Approx <- na.approx(DF$Approx)
plot(DF$Irri, ylim = c(0,max(DF$Irri)+500) )
lines(DF$Approx, col = "red", lwd = 3)
DF$Approx[DOY]


