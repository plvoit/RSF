rm(list = ls())
setwd("~/Workspace/RioSaoFrancisco")
library(rgeos)
library(sf)
library(rgdal)
library(sp)

subbasins <- readOGR("GIS/75subbas-neu/Shape/75SubbasWGS84.shp")
lakes <- readOGR("GIS/Acudes_massa_dagua_v2019/ClippedAcudes/AcudesRSF.shp")
#plot(subbasins)
#plot(lakes, add = T)
summary(lakes)
# without the reservoirs
boxplot(lakes@data[- grep("UHE", lakes@data$nmoriginal),"nuvolumhm3"])

test <- lakes[ grep("UHE", lakes@data$nmoriginal),]


#calculate lake volume with Zés script
modified_molle = function(A,alpha_mod,K_mod,A0=5000) {
  V0=molle(A0)
  V = V0 + A0*((A-A0)/(alpha_mod*K_mod))^(1/(alpha_mod-1)) + K_mod*((A-A0)/(alpha_mod*K_mod))^(alpha_mod/(alpha_mod-1))
  return(V)
}

molle = function(A,alpha=2.7,K=1500) {
  V = K*(A/(alpha*K))^(alpha/(alpha-1))
  return(V)
}


modified_alpha =  function(Pmax,Amax) {
  lambda=Amax/Pmax
  D=Pmax/pi
  return(2.08 + (1.46*10^1)*(lambda/Pmax) - (7.41*10^-2)*(lambda^2/Pmax) - (1.36*10^-8)*(Amax*D/lambda) + 4.07*10^-4*D)
}

modified_K = function(Pmax,Amax) {
  lambda=Amax/Pmax
  D=Pmax/pi
  return(2.55 * 10^3 + (6.45 * 10^1)*lambda - 5.38*10^1*(D/lambda))
}

summary(lakes@data$nuvolumhm3)
summary(lakes@data$nuperimkm)
summary(lakes@data$nuareakm2)


for (i in 1:nrow(lakes@data)){
  if(is.na(lakes@data[i,18])){
    alpha <- modified_alpha(lakes@data[i,20]*1000,lakes@data[i,21]*1e+06)
    K_modified <- modified_K(lakes@data[i,20]*1000,lakes@data[i,21]*1e+06)
    lakes@data[i,18] <-  modified_molle(lakes@data[i,21]*1e+06,alpha,K_modified)/1e+06
    lakes@data$mod_alpha[i] <- alpha
    lakes@data$mod_k[i] <- K_modified
  }
}

#boxplot(lakes@data[- grep("UHE", lakes@data$nmoriginal),"nuvolumhm3"])
#summary(lakes@data$nuvolumhm3)

# for some reason some NAs were generated. Delete these features, as well as the ones which are big reservoirs
lakes <- lakes[ -grep("UHE", lakes@data$nmoriginal),]
lakes <- lakes[!is.na(lakes@data$nuvolumhm3),]
#boxplot(lakes@data[,"nuvolumhm3"])
lakes <- lakes[order(lakes@data$nuvolumhm3, decreasing = T),]

## Some values are incredibly high (higher than reservoirs). I just take them out
lakes <- lakes[lakes@data$nuvolumhm3 <= 50,]
#boxplot(lakes@data[,"nuvolumhm3"])

# take out lakes with negative volume
lakes <- lakes[lakes@data$nuvolumhm3 > 0,]

# just keep necessary columns

lakes@data <- lakes@data[,c(5,18:23)]

# for visual inspection
#hist(lakes@data[lakes@data$nuvolumhm3 > 1.2 & lakes@data$nuvolumhm3 < 15,"nuvolumhm3"], breaks = 20)
#nrow(lakes@data[lakes@data$nuvolumhm3 > 1.2 & lakes@data$nuvolumhm < 15 ,])
# based on visual inspection following groups are suggested
# 0-0.05 7305 elements
# 0.05.-0.3 3970 elements
# 0.3-1.2 1742 elements
#1.2-15 608 elements
# 15 -....  ~  80 elements


## add class information
# based on article by Güntner 2004 Simple water balance modelling of surface reservoir systems in a large data-scarce semiarid region
lakes@data$Reservoir_Class <- cut(lakes@data$nuvolumhm3, c(0,0.1,1,3,10, Inf), labels = F)
hist(lakes@data$Reservoir_Class)

DF <- data.frame("Subbasin" = subbasins@data$DN)

# count the number of lakes for each class in each subbasin 
for( i in 1:nrow(subbasins@data)){
  basin <- subbasins[subbasins$DN == subbasins$DN[i],]
  dummy <- lakes[basin,]
  DF[i,"Class_1"] <- table(dummy$Reservoir_Class)[1]
  DF[i,"Class_2"] <- table(dummy$Reservoir_Class)[2]
  DF[i,"Class_3"] <- table(dummy$Reservoir_Class)[3]
  DF[i,"Class_4"] <- table(dummy$Reservoir_Class)[4]
  DF[i,"Class_5"] <- table(dummy$Reservoir_Class)[5]

}

DF[is.na(DF)] <- 0



table(dummy$Reservoir_Class)[1]
plot(subbasins[2])

# calculate mean lake volume for each class:

Mean_volume <- data.frame("Class_1" = 0, "Class_2" = 0,"Class_3" = 0,"Class_4" = 0,"Class_5" = 0 )
for (i in 1:5){
  dummy <- lakes@data[lakes@data$Reservoir_Class == i,]
  Mean_volume[1,i] <- mean(dummy$nuvolumhm3)
}


hist(test@data[,"nuvolumhm3"], breaks = 200)

sub_lakes <-  list()

for (i in 1:length(subbasins$DN)){
  basin <- subbasins[subbasins$DN == subbasins$DN[i],]
  dummy <- lakes[basin,]
  sub_lakes[[i]] <- dummy@data[,c(18,20,21,23)]
  sub_lakes[[i]]$ID <- as.character(subbasins$DN[i])
}


dummyShape <- subset(subbasins, DN == "2")
plot(dummyShape)
