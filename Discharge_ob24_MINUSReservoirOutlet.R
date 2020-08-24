rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")


Zone_2_change <- c("78", "90", "58", "96", "4") # - out TRES MARIAS
Zone_3_change <-  c("3","2") # - out SOBRADINHO
Zone_4_change <- "1" # - out XINGO

discharge_obs_24 <- read.delim("~/Workspace/RioSaoFrancisco/OrsonInputs/MIT subbas_out INPUT/Zone2/init_config/Input/Time_series/discharge_obs_24.txt", header=T, skip = 4,check.names = F)

Historico_TRES_MARIAS <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Files/Historico_TRES_MARIAS.csv", sep=";", stringsAsFactors = F, dec = ",")
Historico_TRES_MARIAS$Data.da.Medição <- as.POSIXct(Historico_TRES_MARIAS$Data.da.Medição, format = ("%d.%m.%Y"))
Historico_TRES_MARIAS <-  Historico_TRES_MARIAS[Historico_TRES_MARIAS$Data.da.Medição <= "2009-12-31",]
Historico_TRES_MARIAS <-  Historico_TRES_MARIAS[Historico_TRES_MARIAS$Data.da.Medição >= "2000-01-01",]
Historico_TRES_MARIAS$`Defluência..m³.s.` <- gsub(",",".",Historico_TRES_MARIAS$`Defluência..m³.s.`)
Historico_TRES_MARIAS$`Defluência..m³.s.` <- as.numeric(Historico_TRES_MARIAS$`Defluência..m³.s.`)

Historico_SOBRADINHO <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Files/Historico_SOBRADINHO.csv", sep=";", dec = ",")
Historico_SOBRADINHO$Data.da.Medição <- as.POSIXct(Historico_SOBRADINHO$Data.da.Medição, format = ("%d.%m.%Y"))
Historico_SOBRADINHO <-  Historico_SOBRADINHO[Historico_SOBRADINHO$Data.da.Medição <= "2009-12-31",]
Historico_SOBRADINHO <-  Historico_SOBRADINHO[Historico_SOBRADINHO$Data.da.Medição >= "2000-01-01",]
Historico_SOBRADINHO$`Defluência..m³.s.` <- gsub(",",".",Historico_SOBRADINHO$`Defluência..m³.s.`)
Historico_SOBRADINHO$`Defluência..m³.s.` <- as.numeric(Historico_SOBRADINHO$`Defluência..m³.s.`)

Historico_XINGO <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Files/Historico_XINGO.csv", sep=";", dec = ",")
Historico_XINGO$Data.da.Medição <- as.POSIXct(Historico_XINGO$Data.da.Medição, format = ("%d.%m.%Y"))
Historico_XINGO <-  Historico_XINGO[Historico_XINGO$Data.da.Medição <= "2009-12-31",]
Historico_XINGO <-  Historico_XINGO[Historico_XINGO$Data.da.Medição >= "2000-01-01",]
Historico_XINGO$`Defluência..m³.s.` <- gsub(",",".",Historico_XINGO$`Defluência..m³.s.`)
Historico_XINGO$`Defluência..m³.s.` <- as.numeric(Historico_XINGO$`Defluência..m³.s.`)


discharge_obs_24[,Zone_2_change] <- discharge_obs_24[,Zone_2_change] - Historico_TRES_MARIAS$`Defluência..m³.s.`
discharge_obs_24[,Zone_3_change] <- discharge_obs_24[,Zone_3_change] - Historico_SOBRADINHO$`Defluência..m³.s.`
discharge_obs_24[,Zone_4_change] <- discharge_obs_24[,Zone_4_change] - Historico_XINGO$`Defluência..m³.s.`

## change NA to 9999 to be able to make mathematical comparison
discharge_obs_24[is.na(discharge_obs_24) ] <- 9999
discharge_obs_24[discharge_obs_24 < 0 ] <- 0
discharge_obs_24[discharge_obs_24 == 9999 ] <- NA

summary(discharge_obs_24)

#write.table(discharge_obs_24,file ="discharge_obs_24.txt", sep = "\t", row.names = FALSE, quote = FALSE)

withOutput <-  read.delim("~/Workspace/RioSaoFrancisco/Data/Runoff-data/ORIGINAL discharge_obs/discharge_obs_24_NaturalizedFlow.txt",header=T, skip = 4,check.names = F)

#### plotten und Vergleichen der beiden Abflüsse aus den dischargeobs_versionen

for (i in 5:21){
  plot(withOutput[,i], type = "l", main = i)
  lines(discharge_obs_24[,i], type = "l", col = "red")
}

plot(discharge_obs_24[,5]~discharge_obs_24$DD)
