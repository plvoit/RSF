rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco/ResultsCalibration/")

## other cultures demand from Atlas Irrigacao ANA
doy <-  c(1,32,61,92,122,153,183,214,245,275,306,336)
percent_withdraw <- c(0.59,0.69,0.43,0.92,1.42,1.48,1.51,1.61,1.32,0.92,0.62,0.50)
seasonal_others<- data.frame("doy" = c(1:365), "rate" = NA)
seasonal_others[doy,2] <- percent_withdraw

for (i in 1:nrow(seasonal_others)){
  if(is.na(seasonal_others[i,2])) seasonal_others[i,2] <- seasonal_others[i-1,2]
}

n = 10000
x1 <- 0


for ( i in 1:n) 
  y1 <- runif(1,0.2,2)
  x2 <- floor(runif(1,1,365))
  y2 <- runif(1,0.2,2)
  x3 <- floor(runif(1,x2,365))
  y3 <- runif(1,0.2,2)
  x4 <- floor(runif(1,x3,365))
  y4 <- runif(1,0.2,2)
  
  a1 <- (y1-y4)/(365-x4 +x1)
  y0 <- (365-x4) * a1 
  
  res <- data.frame("doy" = c(0:365), y = NA)
  res[c(0,x1,x2,x3,x4),2] <- c(y0,y1,y2,y3,y4)
  
  seasonal_rates <- data.frame("doy" = c(1:365)
  