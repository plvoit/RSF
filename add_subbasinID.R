rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

stations_cal <- read.csv("C:/Users/Admin/Downloads/stations_cal.txt")

stations_cal_ID <- c("F_40050000","F_40100000","TRES MARIAS","F_42210000","F_45298000","F_46035000","F_46360000",
                  "SOBRADINHO","F_48590000","XINGO","Nshift-F_49705000","F_40850000","F_41990000","A_42980000",
                  "F_43880000","F_45260000","F_46902000")


subbas_id <- c(11,10,9,8,7,6,5,4,3,2,1,12,13,14,15,16,17)

stations_subbasinID <- data.frame(ID = stations_cal_ID, subbas = subbas_id)


stations_cal <- merge(stations_cal,stations_subbasinID, by = "ID", all = TRUE)
colnames(stations_cal)[4] <-  "subbas_id"

stations_cal <- stations_cal[order(stations_cal$subbas),]

write.csv(stations_cal,"Data/stations_cal.txt", row.names = FALSE)
