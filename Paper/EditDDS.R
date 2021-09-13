rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
ddsIrriCal <- read.delim("C:/Users/Admin/Desktop/ddsIrriCal.pro")
ddsNoIrri <- read.delim("C:/Users/Admin/Desktop/ddsNoIrri.pro")

ddsIrriCal <- ddsIrriCal[,-c(14,29,43)]

ddsNoIrri[10,c(1:41)] <- ddsIrriCal[10,c(1:41)]

f <- file("ddsNew.pro", "w")
write.table(ddsNoIrri,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)
