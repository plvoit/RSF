# Copyright (C) 2020  Paul Voit

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

vegetation <- read.csv("ResultsCalibration/IrriZones/Zone1newIrri/init_config/Input/Hillslope/vegetation.dat", sep="", skip = 1, header = T)

# modify rootdepth
vegetation[,c(9:12)] <- vegetation[,c(9:12)] * 4
# modify LAI
vegetation[,c(13:16)] <- vegetation[,c(13:16)] * 4

f <- file("vegetation.dat", "w")
writeLines("#Specification of vegetation parameters",f)
write.table(vegetation,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)


