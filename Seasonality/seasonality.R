# fit cyclic linear piecewise regression to DOY-data
# Till Francke, Jan 2014
# input: ASCI-file (infile) with columns of observations for many days in the year (see "seasonal_data.txt")
# output: linear piecewise fit (breakpoints and intercepts) written to seg_fit_coeffs.txt

  #infile="seasonal_data.txt" #input file
  
  
#   infile="seasonal_data_Cfactor-shrubland.txt" #input file
#   infile="seasonal_data_LAI-arable.txt" #input file
#   infile="seasonal_data_Cfactor-arable.txt" #input file
#   
#infiles=c("Irri_season", "seasonal_data_Cfactor-arable.txt") #specify infiles
infiles=c("Irri_season.dat") #specify infiles
#  infiles=dir(pattern = "seasonal.*\\.txt$") #use all files in current dir starting with "seasonal" and extension txt
  
  
  source("seasonality_func.R") #functions to be used

windows()  
for (infile in infiles)  
{  
  set.seed(1) #for reproducable results
  laidata = read.table(file=infile, sep="\t", header=TRUE)
  
  #compute weights of points based on their density (in time)
    cyclic=365
    unique_x = sort(unique(laidata[,1])) #unique values in x
    unique_x = c(unique_x, unique_x[1]+cyclic)
    diffs = diff(unique_x) #distance to next unique data point
    interval_widths = (diffs + diffs[c(length(diffs),1:(length(diffs)-1))]) /2
    #interval_widths = interval_widths[-length(interval_widths)]
    weights= merge(x = cbind(x=laidata[,1]), y = cbind(x=unique_x [-length(unique_x)], w=interval_widths), all.y=FALSE) #assign interval widths (=weights) to individual data points
    laidata$weights=weights$w
  
  #enable this line to disable density-based weighting
  #laidata$weights=1 #set weights to 1
  
  
  n_bp = 4 #number of breakpoints
  init_est=c( quantile(laidata$doy, probs=(1:n_bp)/(n_bp+1)), #initial estimate for breakpoints
            rep(mean(laidata[,2]),4))                         #initial estimate for intercepts
  
  #obj_fun(init_est)
  #optimization using Nelder-Mead: used as baseline and for initial estimate
  res_n=optim(par=init_est, fn=obj_fun)  #faster and better than SANN
  #obj_fun(res_n$par, doplot=TRUE)
    
#optimization using PSO/DDS: slower, but better overall results
  if (!require(ppso))
    install.packages("ppso",repos="http://www.rforge.net/")
  
  tt = aggregate(laidata, by = list(laidata$doy), FUN = mean) #compute mean per specified DOI
  
  
#   #old min/max setting
#   miny = max(c( 0, 0.5*min(laidata[,2])))
#   maxy = 1.2*max(laidata[,2])
  
  #refined min/max setting
  miny = max(c( 0, 0.5*min(tt[,3])))
  maxy = 2*max(tt[,3])
  
  bounds=array(0,c(8,2))
  bounds[1:4,1]=1
  bounds[1:4,2]=365
  bounds[5:8,1]= miny
  bounds[5:8,2]= maxy
  
  
  initial_estimates=res_n$par
  initial_estimates = pmax(initial_estimates, bounds[,1]) #correct initial estimates that are outside bounds
  initial_estimates = pmin(initial_estimates, bounds[,2])
  
  res=optim_dds(number_of_parameters=8, initial_estimates=initial_estimates, objective_function=obj_fun, parameter_bounds=bounds, projectfile=NULL, logfile=NULL )  
  obj_fun(res$par, doplot=TRUE, title=infile) #display fit
  #points(tt$doy, tt[,3], pch="*", col="red") #plot DOI means
  #lines(c(1,365), bounds[5,c(1,1)], col="magenta", lty="dashed") #plot bounds
  #lines(c(1,365), bounds[5,c(2,2)], col="magenta", lty="dashed") #plot bounds
  savePlot(filename = paste0(infile,".png"), type = "png") #save plot as png
  
#write results to seg_fit_coeffs.txt
  logdata = cbind(data.frame(infile=infile), par=t(res$par))
  append=file.exists("seg_fit_coeffs.txt")
  if (append) col.names=FALSE else
    col.names=c("infile", "doy1", "doy2", "doy3", "doy4", "ic1", "ic2", "ic3", "ic4")  
  write.table(file="seg_fit_coeffs.txt", x=logdata, append=append, 
              col.names=col.names, row.names=FALSE, quote=FALSE, sep="\t")
}
  


### Make a nicer plot
library(zoo)

#for segments
Day <-  c(1,31,59,90,120,151,181,212,243,273,304,334,365)
Rate <- c(1114997,1114997,961457,1258302,2056624,2035881,1724569,2001414,1900849,1539738,1059646,740547,800094)


irridata <-  laidata
irridata$Fit <- NA

result <- read.delim("~/Workspace/RioSaoFrancisco/RSF/Seasonality/seg_fit_coeffs.txt")
result[,c(2:5)] <- round(result[,c(2:5)],0)

irridata[1,4] <- 805830.8
irridata[result$doy1,4] <- result$ic4
irridata[result$doy2,4] <- result$ic3
irridata[result$doy3,4] <- result$ic1
irridata[result$doy4,4] <- result$ic2
irridata[365,4] <- 796446.9

irridata$Fit <- na.approx(irridata$Fit)

png(file = "~/Workspace/RioSaoFrancisco/SeasonalIrrigationUrucuia.png", bg = "white", width = 2480, height = 1748, res = 300)
plot(irridata$Irri, type = "h", ylim = (c(0,2200000)), xlab = "DOY", ylab = "", cex.lab = 1, col = "dodgerblue", lwd = 2)
#polygon_x <- c(min(irridata$doy), irridata$doy, max(irridata$doy))
#polygon_y <- c(0, irridata$Irri,0)
#polygon_points <- list(x = polygon_x, y = polygon_y)
#polygon(polygon_points, col = "dodgerblue")

segments(Day, 0, Day, Rate) 
mtext(expression("Irrigation water [m" ^3*"/d]"),side = 2, line = 2, cex = 1)
lines(irridata$Fit, type = "l", col = "red", lwd = 2)
legend("topright", legend = c("Seasonal irrigation", "WASA approximation"), col = c("dodgerblue", "red"), cex = 0.9, lty = 1)
dev.off()
