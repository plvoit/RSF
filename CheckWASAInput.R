
rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

Precipitation <- read.csv("~/Workspace/RioSaoFrancisco/Backupcopy Input WASA/ERA52000_2009_precipitation_at_75subbasins.txt")
SSRD <- read.csv("~/Workspace/RioSaoFrancisco/Backupcopy Input WASA/ERA52000_2009_SSRD_at_75subbasins.txt")
T2m <- read.csv("~/Workspace/RioSaoFrancisco/Backupcopy Input WASA/ERA52000_2009_T2m_at_75subbasins.txt")
rHum <- read.csv("~/Workspace/RioSaoFrancisco/Backupcopy Input WASA/NCEP2000_2009_rHum_at_75subbasins.txt")
