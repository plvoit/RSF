### These script manipulates the shape file of municipios in the RSF basin to add information about irrigation and irrigated area
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

AreaIrrMuncipios <- read.csv("~/Workspace/RioSaoFrancisco/Data/Irrigation/IrrigatedAreaMuncipios.csv", header=FALSE, sep=";", skip = 4, dec = ",", stringsAsFactors = FALSE)

Shape <- read.csv("~/Workspace/RioSaoFrancisco/Data/Irrigation/MuniciosRSF.csv")

#merge data of irrigated area with dbf.file from municipio shapefile
AreaIrrMuncipios <- AreaIrrMuncipios[,-c(11:16)]
AreaIrrMuncipios <- AreaIrrMuncipios[AreaIrrMuncipios$V4 %in% Shape$CD_GEOCMU.C.7,]
AreaIrrMuncipios[,6] <- as.numeric(AreaIrrMuncipios[,6] )
AreaIrrMuncipios[,7] <- as.numeric(AreaIrrMuncipios[,7] )
AreaIrrMuncipios[,8] <- as.numeric(AreaIrrMuncipios[,8] )
AreaIrrMuncipios[,9] <- as.numeric(AreaIrrMuncipios[,9] )
AreaIrrMuncipios[,10] <- as.numeric(AreaIrrMuncipios[,10] )

ShapeMerged <- merge(Shape,AreaIrrMuncipios, by.x = "CD_GEOCMU.C.7", by.y = "V4")
names(ShapeMerged)[14:18] <- c("Rice (flooded)[hec]","Sugar cane [hec]","Other cultures in Central Pivots [hec]","Other cultures and systems [hec]", "Total [hec]")

ShapeMerged <- ShapeMerged[,1:18]
ShapeMerged <- ShapeMerged[,-c(10:13)]
ShapeMerged[is.na(ShapeMerged)] <- 0


## add data about irrigation amounts

DemandMunicipios <- read.csv("~/Workspace/RioSaoFrancisco/Data/Irrigation/DemandMunicipios.csv", header=FALSE, sep=";", dec = ",", stringsAsFactors = F)
DemandMunicipios <- DemandMunicipios[-c(1:3),1:13]
DemandMunicipios <- DemandMunicipios[,-c(1,2,3,5)]

for (i in 2:ncol(DemandMunicipios)){
  
  DemandMunicipios[,i] <- as.numeric(DemandMunicipios[,i])
}

DemandMunicipios[is.na(DemandMunicipios)] <- 0

names(DemandMunicipios)[2:9] <-  c("Rice withdrawal [m3/s]","Rice consumption [m3/s]", "Sugar cane withdrawal [m3/s]", "Sugar cane consumption [m3/s]",
                                   "Others withdrawal [m3/s]", "Others consumption [m3/s]", "Total withdrawal [m3/s]", "Total consumption [m3/s]" )
ShapeMerged <- merge(ShapeMerged, DemandMunicipios, by.x = "CD_GEOCMU.C.7", by.y = "V4")


write.csv(ShapeMerged, "shapenew.csv", row.names = FALSE)
