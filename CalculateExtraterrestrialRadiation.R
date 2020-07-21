rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")


#
# Calculate extra-terrestrial radiation, monthly mean daily values [Wm-2]
# for WASA-SED Input extraterrestrial_radiation.dat
# 
# Source code: Package "sirad"
# 
# Copyright (C) 2019 Anne Müller
#________________________________________________________________________________

rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")

#install.packages("sirad")
library("sirad")

#-------------------------------------------------
# Calculation for several dates (WASA-SED Input)
#-------------------------------------------------

# Latitude in degrees
lat_deg<-(-44)  # - South/ + North (-19.5 = 19.5 degrees South)
# Dates, e.g. 15th of each month of the year
dates<-c("2000-01-15","2000-02-15","2000-03-15","2000-04-15","2000-05-15","2000-06-15","2000-07-15","2000-08-15","2000-09-15","2000-10-15","2000-11-15","2000-12-15")


# Calculate extraterrestrial radiation in [MJm-2]
exra_MJ_dates<-extrat(dayOfYear(dates),radians(lat_deg))
exra_MJ_dates$ExtraTerrestrialSolarRadiationDaily #Daily extraterrestrial radiation in [MJm-2]

# Conversion to [Wm-2] as needed for WASA-SED
exra_W_dates<-(exra_MJ_dates$ExtraTerrestrialSolarRadiationDaily)*(10^6)/86400 #for average watts of 1 day, multiply by 10^6 to get J, divide by 86400 s/day
# exra_W_dates #Daily extraterrestrial radiation in [Wm-2] of specified dates

# Result
round(exra_W_dates, digits = 0 )

## we assume constant extraterrestrial radiation, therefore a monthly value is enough
# the output of round gets copied into a .dat - file by hand