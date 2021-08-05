rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

daily <- read.csv("Tests/rain_daily.dat", sep="\t", skip = 2, header = T, check.names = F )

hours <- data.frame( "0" = rep(0,nrow(daily)*24))
hours[,2] <-  seq(1:nrow(hours))

# fill rows with daily date (24 times)
count = 1
for ( i in 1:nrow(daily)){
  hours[c(count:(count+23)),1] <- daily[i,1]
  count <- count + 24
}


count = 1
for ( i in 1:nrow(daily)){
  #take the daily precipitation and randomly split it to the hours
  # create 24 random values (uniform distribtion) that add up to 1
  random_dist <- diff(c(0, sort(runif(23)), 1))
  hours[c(count:(count+23)),3] <- daily[i,3] * random_dist
  count <- count + 24
}

# same for second subbasin
count = 1
for ( i in 1:nrow(daily)){
  #take the daily precipitation and randomly split it to the hours
  # create 24 random values (uniform distribtion) that add up to 1
  random_dist <- diff(c(0, sort(runif(23)), 1))
  hours[c(count:(count+23)),4] <- daily[i,4] * random_dist
  count <- count + 24
}

# round to 2 digits
hours[,c(3,4)] <- round(hours[,c(3,4)],2)

# change column names
colnames(hours) <- c("0","0","69","15")

f <- file("rain_hourly.dat", "w")
writeLines("Hourly total precipitation [mm] for each subasin, ordered according to Map-IDs",f)
writeLines("Date    Subbasin-ID.s",f)
write.table(hours,file =f, sep = "\t", row.names = FALSE, quote = FALSE)
close(f)
