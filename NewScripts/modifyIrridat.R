rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

irri <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/AllSensZ2/Rate0.5/init_config/Input/Hillslope/irri.dat", header=T, comment.char="#", skip = 1)

irri[,c(5:8)] <- round(0.5 * irri[,c(5:8)],0)

f <- file("irri.dat", "w")
writeLines("# Specification of irrigation operations",f)
write.table(irri,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)

curr_obj_fun_val_day <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/LakesZ2/thread1_best/curr_obj_fun_val_day.txt", sep="", skip = 3, header = F)
