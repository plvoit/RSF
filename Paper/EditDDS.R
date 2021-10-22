rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
ddsIrriCal <- read.delim("C:/Users/Admin/Desktop/ddsIrriCal.pro")
ddsNoIrri <- read.delim("C:/Users/Admin/Desktop/ddsNoIrriNew.pro")

ddsIrriCal <- ddsIrriCal[,-c(14,29,43)]

ddsNoIrri[6,c(1:41)] <- ddsIrriCal[14,c(1:41)]

f <- file("ddsNew.pro", "w")
write.table(ddsNoIrri,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)


# 
# f <- file("ddsNewtest.pro", "w")
# write.table(ddsIrriCal,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
# close(f)
#for test2
ddslog <- read.delim("C:/Users/Admin/Desktop/dds.log")
ddslog <- ddslog[,-15]

f <- file("dds.log", "w")
write.table(ddslog,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)
