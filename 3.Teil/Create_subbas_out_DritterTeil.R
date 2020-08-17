rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

Sobradinho_out <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Processed/Sobradinho_Defluencia.txt", stringsAsFactors = FALSE, dec = ",")
Sobradinho_out$Date <- as.POSIXct(Sobradinho_out$Date, format = c("%d.%m.%Y"))

names(Sobradinho_out)[1] <- "SOBRADINHO"
Sobradinho_out <- Sobradinho_out[,c(2,1)]

# rename gauges with Subbasin IDs from WASA
SubbasID <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")

for (i in 2:ncol(Sobradinho_out)){
  names(Sobradinho_out)[i] <- SubbasID[match(names(Sobradinho_out)[i], SubbasID$Gauges),"Subbas_ID"]
}

# take the dates from 2000-2009
Sobradinho_out<- Sobradinho_out[Sobradinho_out$Date >= "2000-01-01" & Sobradinho_out$Date <= "2009-12-31", ]

Sobradinho_out$Date <- format(Sobradinho_out$Date, "%d%m%Y")

#add timestep column 
Sobradinho_out$Timestep <- "1"

Sobradinho_out <- Sobradinho_out[,c(1,3,2)]
names(Sobradinho_out)[c(1,2)] <- "0"

#theres one NA that needs to be interpolated
#Sobradinho_out$'2' <-  na.approx(Sobradinho_out$'2')
#names(Sobradinho_out)[1] <- "0"

#save file
#write.table(Sobradinho_out,file ="subbasin_out.dat", sep = "\t", row.names = FALSE, quote = FALSE)

## add header manually
## join Sobradinho and XINGO 

XINGO <- read.delim("~/Workspace/RioSaoFrancisco/WASA-SED/0/X4/Input/Time_series/subbasin_out.dat", skip = 2, check.names = FALSE)

Zone3_out <- Sobradinho_out
Zone3_out["2"] <- XINGO["2"]
write.table(Zone3_out,file ="subbasin_out.dat", sep = "\t", row.names = FALSE, quote = FALSE)
