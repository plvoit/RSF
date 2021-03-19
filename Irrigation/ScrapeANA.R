

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library("jsonlite")
     
ID <- read.csv2("~/Workspace/RioSaoFrancisco/Data/Irrigation/ScrapeANA/ObjectID.txt", sep=",", header = F)

ID <- as.numeric(ID)

#URL <- c("https://www.snirh.gov.br/arcgis/rest/services/SRE/Outorgas_de_Direito_de_Uso/MapServer/0/query?f=json&where=&returnGeometry=true&spatialRel=esriSpatialRelIntersects&objectIds=17&outFields=*&outSR=102100")
DF <- data.frame()

for ( i in 10402:length(ID)){
URL <- paste0("https://www.snirh.gov.br/arcgis/rest/services/SRE/Outorgas_de_Direito_de_Uso/MapServer/0/query?f=json&where=&returnGeometry=true&spatialRel=esriSpatialRelIntersects&objectIds=",ID[i],"&outFields=*&outSR=102100")  
dummy <- fromJSON(URL)
DF  <- rbind(DF,dummy)
}

write.csv(DF,"LicencesRSFBasin.txt")
