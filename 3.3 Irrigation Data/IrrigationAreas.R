# ### These script manipulates the shape file of municipios in the RSF basin to add information about irrigation and irrigated area
# 
# # Copyright (C) 2020 Paul Voit
# rm(list = ls())
# library(foreign) # for reading .dbf files
# library(readxl)
# setwd("~/Workspace/RioSaoFrancisco")
# 
# AreaIrrMuncipios <- read.csv("~/Workspace/RioSaoFrancisco/Data/Irrigation/IrrigatedAreaMuncipios.csv", header=FALSE, sep=";", skip = 4, dec = ",", stringsAsFactors = FALSE)
# 
# Shape <- read.dbf("GIS/Municipalities2015/WGS8423S_meters/Clipped_Municipios.dbf")
# 
# #merge data of irrigated area with dbf.file from municipio shapefile
# AreaIrrMuncipios <- AreaIrrMuncipios[,-c(11:16)]
# AreaIrrMuncipios <- AreaIrrMuncipios[AreaIrrMuncipios$V4 %in% Shape$CD_GEOCMU,]
# AreaIrrMuncipios[,6] <- as.numeric(AreaIrrMuncipios[,6] )
# AreaIrrMuncipios[,7] <- as.numeric(AreaIrrMuncipios[,7] )
# AreaIrrMuncipios[,8] <- as.numeric(AreaIrrMuncipios[,8] )
# AreaIrrMuncipios[,9] <- as.numeric(AreaIrrMuncipios[,9] )
# AreaIrrMuncipios[,10] <- as.numeric(AreaIrrMuncipios[,10] )
# 
# ShapeMerged <- merge(Shape,AreaIrrMuncipios, by.x = "CD_GEOCMU", by.y = "V4")
# names(ShapeMerged)[14:18] <- c("Rice (flooded)[hec]","Sugar cane [hec]","Other cultures in Central Pivots [hec]","Other cultures and systems [hec]", "Total [hec]")
# 
# ShapeMerged <- ShapeMerged[,1:18]
# ShapeMerged <- ShapeMerged[,-c(10:13)]
# ShapeMerged[is.na(ShapeMerged)] <- 0
# 
# 
# ## add data about irrigation amounts -> Better open as excel file (below)
# 
# #DemandMunicipios <- read.csv("~/Workspace/RioSaoFrancisco/Data/Irrigation/DemandMunicipios.csv", header=FALSE, sep=";", dec = ",", stringsAsFactors = F)
# #DemandMunicipios <- DemandMunicipios[-c(1:3),1:13]
# #DemandMunicipios <- DemandMunicipios[,-c(1,2,3,5)]
# 
# #for (i in 2:ncol(DemandMunicipios)){
#   
# #  DemandMunicipios[,i] <- as.numeric(DemandMunicipios[,i])
# #}
# 
# #DemandMunicipios[is.na(DemandMunicipios)] <- 0
# 
# #names(DemandMunicipios)[2:9] <-  c("Rice withdrawal [m3/s]","Rice consumption [m3/s]", "Sugar cane withdrawal [m3/s]", "Sugar cane consumption [m3/s]",
# #Others withdrawal [m3/s]", "Others consumption [m3/s]", "Total withdrawal [m3/s]", "Total consumption [m3/s]" )
# 
# DemandMunicipios <- read_excel("Data/Irrigation/_ATLAS_Irrigacao_DemandaHidrica_2015_2030_ANA_Metadados_env_v2.xlsx", col_names = TRUE, skip = 2)
# DemandMunicipios <- DemandMunicipios[-c(1:3),1:13]
# DemandMunicipios <- DemandMunicipios[,-c(1,2,3,5)]
# 
# names(DemandMunicipios)[2:9] <-  c("Rice withdrawal [m3/s]","Rice consumption [m3/s]", "Sugar cane withdrawal [m3/s]", "Sugar cane consumption [m3/s]",
#                                     "Others withdrawal [m3/s]", "Others consumption [m3/s]", "Total withdrawal [m3/s]", "Total consumption [m3/s]" )
# 
# DemandMunicipios[2:9] <-  round(DemandMunicipios[2:9],6)
# ShapeMerged <- merge(ShapeMerged, DemandMunicipios, by.x = "CD_GEOCMU", by.y = "...4")
# 
# 
# write.dbf(ShapeMerged, "Clipped_Municipios.dbf")
# 
# ### extend shape with fixed irrigation rates gained from water use licences

rm(list = ls())
library(foreign) # for reading .dbf files
library(readxl)
setwd("~/Workspace/RioSaoFrancisco")

Shape <- read.dbf("GIS/75subbas-neu/Shape/75SubbasWGS84.dbf")

#get fixed rates from irri.dat

irri <- read.delim("ResultsCalibration/Mod/AllZ2/init_config/Input/Hillslope/irri.dat", comment.char="#")

# convert seasonal to mean
for(i in 1:nrow(irri)){
  dummy <- round(rowMeans(irri[i,c(5:8)]),0)
  irri[i,c(5:8)] <- dummy
}

# sum up lake and river irri rates
#new <- aggregate(irri, by = list(irri$sub_source), FUN = sum)

lakeirri <- irri[48:67,]
riverirri <- irri[1:47,]

for( i in 1:nrow(riverirri)){
  dummy <- lakeirri[match(riverirri[i,1],lakeirri[,1]),]
  if(!is.na(dummy[1,1])){
    riverirri[i,c(5:8)] <- riverirri[i,c(5:8)] + dummy[1,c(5:8)]
  }
}

# extend dbf from shapefile
Shape$mean_irri <- 0

for ( i in 1:nrow(Shape)){
  dummy <- riverirri[match(Shape[i,1],riverirri[,1]),]
  if(!is.na(dummy[1,1])) Shape[i,5] <- dummy[1,5]
}

# write extended dbf
write.dbf(Shape, "75SubbasWGS84.dbf")
