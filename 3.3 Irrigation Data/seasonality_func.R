#functions for segmented/piecewise cyclic linear regression
#(package segmented proved unstable for this)
# Till Francke, Jan 2014


# #make artificial data
#   n_scenes=12 #number of scenes / days in year with LAI information
#   points_per_scene=50 #number of data points per scene
#   laidata=data.frame()
#   for (i in 1:n_scenes)
#   {
#     doy=round(runif(1, min=1, max=365))
#     laidata=rbind(laidata, 
#                   data.frame(doy=doy, lai=rnorm(n=points_per_scene, mean=-cos(doy/365*2*pi), sd=0.3)))
#   
#   }
#   summary(laidata)
#   
#   sort(unique(laidata$doy))
#   
#   plot(laidata$doy, laidata$lai)
#   write.table(laidata, file="seasonal_data.txt", sep="\t", quote=F, row.names=FALSE)
  
  
segmented_sse = function(data, breakpoints, intercepts, cyclic=NA, doplot=FALSE, title="")
  #return sum of squared errors, weighted by number of data, for linear regression (data[,2]~data[,1]) 
  # within all intervalls specified by breakpoints
  #cyclic=100 wraps the last interval around at 100 (spans an interval between last and first breakpoint)
{
  breakpoints = sort(breakpoints)
    if (any (diff(breakpoints) ==0)) return (Inf)  #convergence of breakpoints not allowed
    
  if (!is.na(cyclic)) #create cyclicity
  {  
    before_first_breakpoint = data [data[,1]<=breakpoints[1] ,]  #all data before first bp
    before_first_breakpoint[,1] = before_first_breakpoint[,1] + cyclic #shift predictor (x) of this slice by "cyclic"
    data    = rbind(data   , before_first_breakpoint) #append shifted first slice to end of data
    
    breakpoints = c(breakpoints, breakpoints[1] + cyclic) #shift first breakpoint and append to end of list
    intercepts  = c(intercepts , intercepts[1]) #shift first intercept and append to end of list
  }

  
  if (doplot)
  {
    ylim=c(0, max(c(data[,2], intercepts)))
    if (is.na(cyclic))
      plot(data[,1], data[,2], ylim=ylim, main=title)
    else
    {  
      plot(data[,1], data[,2], xlim=c(0, cyclic), ylim=ylim, main=title, xlab="DOY [-]", ylab="value [-]")
      #points(before_first_breakpoint[,1], before_first_breakpoint[,2], col="red")
      nbp=length(breakpoints)
      lines(x=c(breakpoints[nbp-1]-cyclic, breakpoints[1]), y=intercepts[c(nbp-1,1)], col="green")
    }  
    points(x=breakpoints, y=intercepts, col="green")
    lines(x=breakpoints, y=intercepts, col="green")
    legend("topright", legend=c("data", "fit"), lty=c(0,1), pch=21, col=c("black","green"))
  }
  
  
  sse=0
  for (i in 1:(length(breakpoints)-1))  #treat all intervals between breakpoints
  {
          
    data_idx = (data[,1]>= breakpoints[i  ]) &
               (data[,1]<= breakpoints[i+1])   #data in current interval
    if (!any(data_idx))
        return(Inf)
        
    resids = data[data_idx,2] - approx(x=breakpoints[i:(i+1)], y=intercepts[i:(i+1)], xout=data[data_idx,1])$y
    if (ncol(data)>2)
      resids = resids * data[data_idx,3] #do weighting, if present
    sse = sse + sum(resids^2)
  }
  
  return(sse)     
}
  
  
obj_fun = function(parms, doplot=FALSE, title="") #objective function used in optimization
{
  return(segmented_sse(data=laidata, breakpoints=parms[  1:(length(parms)/2) ],
                                    intercepts  =parms[-(1:(length(parms)/2))], cyclic=365, doplot=doplot, title=title))
}  
