rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

irri <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/AllSensZ1/init_config/Input/Hillslope/irri.dat", header=T, comment.char="#", skip = 1)

irri[,c(5:8)] <- round(0.5 * irri[,c(5:8)],0)

f <- file("irri.dat", "w")
writeLines("# Specification of irrigation operations",f)
write.table(irri,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)

curr_obj_fun_val_day <- read.csv("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/LakesZ2/thread1_best/curr_obj_fun_val_day.txt", sep="", skip = 3, header = F)


####
irri$rule <-  'cwd'
irri[,c(5:8)] <- 0.0845

f <- file("irri.dat", "w")
writeLines("# Specification of irrigation operations",f)
write.table(irri,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)


##for CWD irri_seasons must exist

seasons <- read.delim("~/Workspace/RioSaoFrancisco/ResultsCalibration/Mod/AllSensZ1/init_config/Input/Hillslope/irri_seasons.dat", header=T, comment.char="#", skip = 1)
seasons_names <-  names(seasons)

seasons  <-  data.frame("subbasin_id" = irri$sub_source)
seasons$year <- -1
seasons$DOY1 <- 1
seasons$DOY2 <- 2
seasons$DOY3 <- 3
seasons$DOY4 <- 4


f <- file("irri_seasons.dat", "w")
writeLines("# Specification of the irrigation seasonality (per year)",f)
writeLines("# for the interpolation of temporal distribution irrigation water",f)
write.table(seasons,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)