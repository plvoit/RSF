setwd("~/Workspace")
svc <- read.csv("~/Workspace/Zone3AIrrigation/Input/Hillslope/svc.dat", sep="\t", skip = 2, header = FALSE)

svc$V9 <-  0
svc[svc$V3 == 50, 9] <- 1

write.table(svc,file ="svc.dat", sep = "\t", row.names = FALSE, quote = FALSE, col.names = FALSE)
