### These script manipulates the shape file of municipios in the RSF basin to add information about irrigation and irrigated area
rm(list = ls())
library(foreign) # for reading .dbf files
library(readxl)
setwd("~/Workspace/RioSaoFrancisco")

Shape <- read.dbf("GIS/Urucuia/UrucuiaWaterAbstraction.dbf")

V_annual <- sum(Shape$Volume.Anu) # m^3/y
V_daily <- V_annual / 365

V_daily  / 24 / 3600
