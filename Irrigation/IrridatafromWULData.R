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


IrriSub <- over(subbasins,licencesShape, returnList = T)

#irridat <- data.frame("sub_source" = , "source" = , "sub_receiver" = , "rule" = "seasonal",
#                      "rate" = , "rate2" = , "rate3" = ),
#                      "rate4" = , loss_factor = 0.8)

DF <- data.frame()
# get monthly irrigation sums for each subbasin
for (i in 1:length(subbasins$DN)){
  sources <-  levels(s)
}

dummy <- IrriSub[[1]]
dummy <- dummy[dummy$FINALIDADE_PRINCIPAL == "Irrigação",]

i = 1

sources <- unique(dummy$CORPO_HIDRICO) 
for ( i in 1:length(sources)){
  dummy2 <- dummy[dummy$CORPO_HIDRICO == sources[i],]
  dummy2 <- dummy2[,-c(1,2)]
  # calculate the daily volume of water abstracted (m^3/d) (daily Volume * days per month * hours per day)
  month_DF <- data.frame("Jan" = (dummy2[,19] * dummy2[,33] * dummy2[,34])/ 31)
  month_DF$Feb <- (dummy2[,20] * dummy2[,35] * dummy2[,36])/ 28
  month_DF$Mar <- (dummy2[,21] * dummy2[,37] * dummy2[,38])/ 31
  month_DF$Apr <- (dummy2[,22] * dummy2[,39] * dummy2[,40])/ 30
  month_DF$May <- (dummy2[,23] * dummy2[,41] * dummy2[,42])/ 31
  month_DF$Jun <- (dummy2[,24] * dummy2[,43] * dummy2[,44])/ 30
  month_DF$Jul <- (dummy2[,25] * dummy2[,45] * dummy2[,46])/ 31
  month_DF$Aug <- (dummy2[,26] * dummy2[,47] * dummy2[,48])/ 31
  month_DF$Sep <- (dummy2[,27] * dummy2[,49] * dummy2[,50])/ 30
  month_DF$Oct <- (dummy2[,28] * dummy2[,51] * dummy2[,52])/ 31
  month_DF$Nov <- (dummy2[,29] * dummy2[,53] * dummy2[,54])/ 30
  month_DF$Dec <- (dummy2[,30] * dummy2[,55] * dummy2[,56])/ 31
}