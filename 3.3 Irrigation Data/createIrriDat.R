## These script is for creating irri.dat and irri_seasons.dat.

# Copyright (C) 2020 Paul Voit

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

library(foreign)

savefile = TRUE
irri_seasons = TRUE

Sub <- read.dbf("GIS/75subbas-neu/WGS8423S_for_meters/75Subbas_meters.dbf")
Zone <- 1   #Select Zone

Sub <-  Sub[Sub$Zone == Zone,]

### this version has constant irrigation all year round

irridat <- data.frame("sub_source" = Sub$DN, "source" = "river", "sub_receiver" = Sub$DN, "rule" = "fixed",
                      "rate" = round(Sub$tot_withdr * 86400,0), "rate2" = round(Sub$tot_withdr * 86400,0), "rate3" = round(Sub$tot_withdr * 86400,0),
                      "rate4" = round(Sub$tot_withdr * 86400,0), loss_factor = 0.8)

if (savefile){
f <- file("irri.dat", "w")
writeLines("# Specification of irrigation operations",f)
write.table(irridat,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)
}

## look at efficency
df <- data.frame("withdraw" = Sub$tot_withdr, "consumption" = Sub$Consumptio)
df$efficency <- df$consumption / df$withdraw 
summary(df)

# create seasonality from mean values

## other cultures demand from Atlas Irrigacao ANA
doy <-  c(1,32,61,92,122,153,183,214,245,275,306,336)
percent_withdraw <- c(0.59,0.69,0.43,0.92,1.42,1.48,1.51,1.61,1.32,0.92,0.62,0.50)
seasonal_others<- data.frame("doy" = c(1:365), "rate" = NA)
seasonal_others[doy,2] <- percent_withdraw

for (i in 1:nrow(seasonal_others)){
  if(is.na(seasonal_others[i,2])) seasonal_others[i,2] <- seasonal_others[i-1,2]
}


## try to imitate this pattern with just four points in time
seasonality_points <-  c(1,59,121,212,366)
rate_factors <-  c(0.5,0.43,1.42, 1.61,0.5)
sum(rate_factors)

seasonal_rates <- data.frame("doy" = c(1:365), "rate" = NA)
seasonal_rates[seasonality_points,2] <- rate_factors

library(zoo)
seasonal_rates[,2] <- na.approx(seasonal_rates[,2])

plot(seasonal_others$rate~seasonal_others$doy, type = "l")
lines(seasonal_rates$rate~seasonal_rates$doy , type = "l", col = "red")
#legend("topleft", legend = c("Seasonality ANA","modeled seasonality"), col = c("black","red"))

int_rates <-  sum(seasonal_rates$rate)
int_ANA <- sum(seasonal_others$rate)


# write new irri.dat
irridat <- data.frame("sub_source" = Sub$DN, "source" = "river", "sub_receiver" = Sub$DN, "rule" = "seasonal",
                      "rate" = round(Sub$tot_withdr * 86400 * rate_factors[1],0), "rate2" = round(Sub$tot_withdr * 86400 * rate_factors[2],0), "rate3" = round(Sub$tot_withdr * 86400 * rate_factors[3],0),
                      "rate4" = round(Sub$tot_withdr * 86400 * rate_factors[4],0), loss_factor = round(df$efficency,2))

if (savefile){
  f <- file("irri.dat", "w")
  writeLines("# Specification of irrigation operations",f)
  write.table(irridat,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
  close(f)
}

# create according irri_seasons.dat

df <- data.frame("subbasin_id" = irridat$sub_receiver, "year" = -1, DOY1 = seasonality_points[1], DOY2 = seasonality_points[2],
                 DOY3 = seasonality_points[3], DOY4 = seasonality_points[4])

if(irri_seasons){
  
  f <- file("irri_seasons.dat", "w")
  writeLines("# Specification of the irrigation seasonality (per year)",f)
  writeLines("# for the interpolation of temporal distribution irrigation water",f)
  write.table(df,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
  close(f)
}



