rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(zoo)

XINGO_Defluencia <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Processed/Xingo_Defluencia.txt", stringsAsFactors = FALSE, dec = ",")
XINGO_Defluencia$Date <- as.POSIXct(XINGO_Defluencia$Date, format = c("%d.%m.%Y"))

names(XINGO_Defluencia)[1] <- "XINGO"
XINGO_Defluencia <- XINGO_Defluencia[,c(2,1)]

# rename gauges with Subbasin IDs from WASA
SubbasID <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")

for (i in 2:ncol(XINGO_Defluencia)){
  names(XINGO_Defluencia)[i] <- SubbasID[match(names(XINGO_Defluencia)[i], SubbasID$Gauges),"Subbas_ID"]
}

# take the dates from 2000-2009
XINGO_Defluencia<- XINGO_Defluencia[XINGO_Defluencia$Date >= "2000-01-01" & XINGO_Defluencia$Date <= "2009-12-31", ]

XINGO_Defluencia$Date <- format(XINGO_Defluencia$Date, "%d%m%Y")

#add timestep column 
XINGO_Defluencia$Timestep <- "1"

XINGO_Defluencia <- XINGO_Defluencia[,c(1,3,2)]
names(XINGO_Defluencia)[c(1,2)] <- "0"

#theres one NA that needs to be interpolated
XINGO_Defluencia$'2' <-  na.approx(XINGO_Defluencia$'2')
summary(XINGO_Defluencia)

#save file
write.table(XINGO_Defluencia,file ="subbasin_out.dat.txt", sep = "\t", row.names = FALSE, quote = FALSE)

## add header manually