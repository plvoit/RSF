## This script is changing the observed riverflow at the reservoirs from naturalized flow
## to reservoir inflow. This is done because now the zones are separated and the naturalized
## flow isn't used anymore

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

discharge_obs_24 <- read.delim("~/Workspace/RioSaoFrancisco/WASA-SED/0/1.2/Input/Time_series/discharge_obs_24.txt", header=FALSE, skip = 5)

# load reservoir data

SOBRADINHO <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Files/Historico_SOBRADINHO.csv", sep=";")
XINGO <- read.csv("~/Workspace/RioSaoFrancisco/Data/ONS reservoir Data/Files/Historico_XINGO.csv", sep=";")

XINGO$Data.da.Medição <- as.POSIXct(XINGO$Data.da.Medição, format = c("%d.%m.%Y"))
SOBRADINHO$Data.da.Medição <- as.POSIXct(SOBRADINHO$Data.da.Medição, format = c("%d.%m.%Y"))

##2000-2009
XINGO <- XINGO[XINGO$Data.da.Medição <= "2009-12-31" & XINGO$Data.da.Medição >= "2000-01-01", ]
SOBRADINHO <- SOBRADINHO[SOBRADINHO$Data.da.Medição <= "2009-12-31" & SOBRADINHO$Data.da.Medição >= "2000-01-01", ]

### subbas 4 = Sobradinho
## subbas 2 = Xingo

discharge_obs_24$V5 <-  SOBRADINHO$`Afluência..m³.s.`
discharge_obs_24$V7 <- XINGO$`Afluência..m³.s.`

#save file
write.table(discharge_obs_24,file ="discharge_obs_24.txt", sep = "\t", row.names = FALSE, quote = FALSE)

discharge_obs_24 <- read.delim("~/Workspace/RioSaoFrancisco/WASA-SED/0/X5/Input/Time_series/discharge_obs_24.txt", header=FALSE, skip = 1)
summary(discharge_obs_24)

#discharge_obs_24[,c(5,7)] <- gsub(",",".",discharge_obs_24[,c(5,7)])
#geht so nicht