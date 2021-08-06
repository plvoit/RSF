rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

tres <- read.csv("Data/ONS reservoir Data/Processed/TRES MARIAS_Defluencia.txt", dec=",")#
tres$Date <- as.POSIXct(tres$Date, format="%d.%m.%Y")
tres <- tres[tres$Date >= '2010-01-01',]
tres <- tres[tres$Date < '2016-01-01',]
# change date format
tres$Date  <- format(tres$Date, "%d%m%Y")

tres$timestep <- 1
#change order of columns
tres <- tres[,c(2,3,1)]

names(tres) <- c('0','0','9')



f <- file("subbasin_out.dat", "w")
writeLines("pre-specified mean daily river flow [m3/s] for selected sub-basins (MAP-IDs)	",f)
writeLines('Date	Timestep	Subbasin-ID.', f)
write.table(tres,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)
