rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

TRES.MARIAS_Defluencia <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Processed/TRES MARIAS_Defluencia.txt", stringsAsFactors = FALSE, dec = ",")
TRES.MARIAS_Defluencia$Date <- as.POSIXct(TRES.MARIAS_Defluencia$Date, format = c("%d.%m.%Y"))

names(TRES.MARIAS_Defluencia)[1] <- "TRES MARIAS"
TRES.MARIAS_Defluencia <- TRES.MARIAS_Defluencia[,c(2,1)]

# rename gauges with Subbasin IDs from WASA
SubbasID <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")

for (i in 2:ncol(TRES.MARIAS_Defluencia)){
  names(TRES.MARIAS_Defluencia)[i] <- SubbasID[match(names(TRES.MARIAS_Defluencia)[i], SubbasID$Gauges),"Subbas_ID"]
}

# take the dates from 2000-2009
TRES.MARIAS_Defluencia<- TRES.MARIAS_Defluencia[TRES.MARIAS_Defluencia$Date >= "2000-01-01" & TRES.MARIAS_Defluencia$Date <= "2009-12-31", ]



TRES.MARIAS_Defluencia$Date <- format(TRES.MARIAS_Defluencia$Date, "%d%m%Y")

names(TRES.MARIAS_Defluencia)[1] <- "0"

#save file
write.table(TRES.MARIAS_Defluencia,file ="subbasin_out.dat.txt", sep = "\t", row.names = FALSE, quote = FALSE)

## add header manually