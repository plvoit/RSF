rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(foreign)

Sub <- read.dbf("GIS/75subbas-neu/WGS8423S_for_meters/75Subbas_meters.dbf")

Zone <- 1

Sub <-  Sub[Sub$Zone == 1,]

irridat <- data.frame("sub_source" = Sub$DN, "source" = "river", "sub_receiver" = Sub$DN, "rule" = "fixed",
                      "rate" = round(Sub$Consumptio * 86400,0), "rate2" = round(Sub$Consumptio * 86400,0), "rate3" = round(Sub$Consumptio * 86400,0),
                      "rate4" = round(Sub$Consumptio * 86400,0), loss_factor = 0.8)




f <- file("irri.dat", "w")
writeLines("# Specification of irrigation operations",f)
write.table(irridat,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)
