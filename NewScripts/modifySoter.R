rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

soter <- read.delim("ResultsCalibration/Alluv/Z2/init_config/Input/Hillslope/soter.dat", header=T, comment.char="#", skip = 1)

soter[,9] <- 3000

f <- file("soter.dat", "w")
writeLines("# Specification of landscape units",f)
write.table(soter,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)


test <- soter[,ncol(soter)-4]
