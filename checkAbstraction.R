rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/Data")

ResandRates <- read.delim2("~/Workspace/RioSaoFrancisco/Data/Irrigation/ResandRates.txt")

plot(ResandRates$tot_with~ResandRates$res)


base <- c(0.29,0.48,-0.11,0.47,-0.43,-0.61,-0.95,0.2,-0.15,-0.6,-6.2,-11.3,0.87,0.44,0.59)
irri <- c(0.29,0.48,-0.12,0.5,-0.25,-0.35,-0.64,0.24,-0.04,-0.4,-4.2,-7.2,0.87,0.44,0.59)

lm()
anova()
t.test(base,irri)


base <- c(102.4,162.4,117.2,569.3,1672.5,1802.9,1949.9,287.8,520.1,290.8,107.0,250.8,245,721.2,494.3)
irri <- c(102.6,162.5,117.5,550.1,1566.2,1649.4,1788.9,280.3,495.6,271.8,90.7,204.9,245.4,722.6,495.1)

t.test(base,irri)


# check abstracted vs supplied irri water


rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/IrriZones")
df <-  data.frame()
i = 3
for ( i in 1: length(dir())){
  
  supply <- read.table(paste(dir()[i],"/thread1_best/Output/irrigation_abstraction.out",sep = ""), quote="\"", comment.char="", skip = 1, header = T, check.names = F)
  rows <- nrow(supply) 
  supply <- apply(supply[,4:ncol(supply)],2,sum)
  demand <- read.table(paste(dir()[i],"/thread1_best/Input/Hillslope/irri.dat",sep = ""), quote="\"", comment.char="", skip = 1, header = T)
  demand <- demand[,c(1,5)]
  
  for (j in 1:length(supply)){
    supply[j] <- supply[j] - demand[match(names(supply)[j],demand[,1]),2] * rows
  }
  
}

