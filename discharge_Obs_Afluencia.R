rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

discharge_obs_24 <- read.delim("~/Workspace/RioSaoFrancisco/OrsonInputs/MIT subbas_out INPUT/Zone1/init_config/Input/Time_series/discharge_obs_24.txt", header=T, skip = 4,check.names = F, stringsAsFactors = T)

Historico_TRES_MARIAS <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Files/Historico_TRES_MARIAS.csv", sep=";", stringsAsFactors = F)
Historico_TRES_MARIAS$Data.da.Medição <- as.POSIXct(Historico_TRES_MARIAS$Data.da.Medição, format = ("%d.%m.%Y"))
Historico_TRES_MARIAS <-  Historico_TRES_MARIAS[Historico_TRES_MARIAS$Data.da.Medição <= "2009-12-31",]
Historico_TRES_MARIAS <-  Historico_TRES_MARIAS[Historico_TRES_MARIAS$Data.da.Medição >= "2000-01-01",]


Historico_SOBRADINHO <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Files/Historico_SOBRADINHO.csv", sep=";",stringsAsFactors = F)
Historico_SOBRADINHO$Data.da.Medição <- as.POSIXct(Historico_SOBRADINHO$Data.da.Medição, format = ("%d.%m.%Y"))
Historico_SOBRADINHO <-  Historico_SOBRADINHO[Historico_SOBRADINHO$Data.da.Medição <= "2009-12-31",]
Historico_SOBRADINHO <-  Historico_SOBRADINHO[Historico_SOBRADINHO$Data.da.Medição >= "2000-01-01",]

Historico_XINGO <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Files/Historico_XINGO.csv", sep=";",stringsAsFactors = F)
Historico_XINGO$Data.da.Medição <- as.POSIXct(Historico_XINGO$Data.da.Medição, format = ("%d.%m.%Y"))
Historico_XINGO <-  Historico_XINGO[Historico_XINGO$Data.da.Medição <= "2009-12-31",]
Historico_XINGO <-  Historico_XINGO[Historico_XINGO$Data.da.Medição >= "2000-01-01",]

discharge_obs_24$`9` <-   Historico_TRES_MARIAS$`Afluência..m³.s.`
discharge_obs_24$`4` <-   Historico_SOBRADINHO$`Afluência..m³.s.`
discharge_obs_24$`2` <-   Historico_XINGO$`Afluência..m³.s.`

discharge_obs_24[,c(5,6,7)] <- gsub(",",".",discharge_obs_24[,c(5,6,7)])

gsub(",",".",discharge_obs_24$`4`)

write.table(discharge_obs_24,file ="discharge_obs_24.txt", sep = "\t", row.names = FALSE, quote = FALSE)
