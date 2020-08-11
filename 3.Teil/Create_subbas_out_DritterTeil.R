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

names(Sobradinho_out)[1] <- "0"

#save file
write.table(Sobradinho_out,file ="subbasin_out.dat.txt", sep = "\t", row.names = FALSE, quote = FALSE)

## add header manually