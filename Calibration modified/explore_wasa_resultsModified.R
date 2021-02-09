#working_dir=rm(list = ls())
rm(list = ls())

setwd("~/Workspace/RioSaoFrancisco")

wasa_input_dir =paste("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/WASA-SED/0","/Input/Time_series",sep="")
wasa_output_dir=paste("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/WASA-SED/0","/Output/",sep="")

run_dir="C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/WASA-SED"     #path/directory of the parametrisation folder, containing input/output files of your modelling example
thread_dir="Zone3new/"   #specifiy the directory (e.g., example1, tutorial, ...)

#WARNING: the ORIGINAL source directory for the files is obtained from WASA-file parameter.out
#this can be fixed by manually modifications near "#REPL"

#########################################
# load clearer names from CheckWASAoutput.R for later labelling the plots
# load GaugeNumber-SubbasID file
SubbasID_GaugeNumber <- read.csv("~/Workspace/RioSaoFrancisco/Data/Runoff-data/SubbasID_GaugeNumber.txt")
SubbasID_GaugeNumber$Gauges <- as.character(SubbasID_GaugeNumber$Gauges)
#write.csv(SubbasID_GaugeNumber, file = "Data/Runoff-data/new.txt", row.names = FALSE)

save_plot=TRUE

############
setwd("C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/wasa_visualisation") #directory/location of R-files for visualisation
source(paste0("read_wasa_func.R"))        #in this location, access "read_wasa_func.R"


################################################

#read WASA runtime parameters, specify path of parameter.out, should be in the Output directory 
ctrl_params = parse_wasa_ctrl_params(wasa_param_file=paste0(run_dir, "/0/",thread_dir,"Output/parameter.out"))

#execute this block if you get Error in file(file, "r") : cannot open the connection"
if (!grepl(ctrl_params$input_dir, pattern="^[a-zA-Z]:|^[/\\]")) #if this is a relative path, prepend working dir
  ctrl_params$input_dir  = paste0(run_dir,ctrl_params$input_dir)
if (!grepl(ctrl_params$output_dir, pattern="^[a-zA-Z]:|^[/\\]")) #if this is a relative path, prepend working dir
  ctrl_params$output_dir = paste0(run_dir,ctrl_params$output_dir)

#Manual fix of wrong relative paths
ctrl_params$input_dir=paste(run_dir, "/0/", thread_dir, "Input/", sep = "")
ctrl_params$output_dir=paste(run_dir, "/0/", thread_dir, "Output/", sep = "")

if (T) #compute runoff coefficients -> only for time period with measured data in discharge_obs_24.txt !! 
{  
  #read WASA simulation results # put the subbasins you want to look at in this vector
  #!!! The same needs to be done in line 261
  # subbas_id = c(10,11,12) #Zone 1
  # subbas_id=c(13,73,78,15,16,90,58,45,96) #Zone2
   subbas_id = c(1,2,3) #Zone3
  
  
  
  res = read_wasa_results(ctrl_params,components_list=c(
    "River_Flow",     # this only works well, if subbasin does not get inflow from upstream subbasins
    "daily_water_subbasin"
  ), subbas_id=subbas_id)  #the strange structure is due to compatibility wiht RHydro package
  #read observations
  #ctrl_params$input_dir <- "C:/Users/Admin/Documents/Workspace/RioSaoFrancisco/WASA-SED/WASA-SED/0/Input" ## Quickfix because the slash doesn't work 
  obs = read_observations(subbas_id=subbas_id, datevec=res$datevec, target_component=c("River_Flow","rain"), # an dieser Stelle bleibt "River_Flow, auch wenn "daily water subbas" ausgew?hlt
                          wasa_input_dir=ctrl_params$input_dir)
  
  t_res = as.numeric(difftime(res[[1]][2], res[[1]][1], units="sec")) #temporal resolution in seconds
  
  data_cols = dimnames(res[[2]])[[2]] #names of WASA result columns
  
  #compute runoff coefficients
  mod_df=data.frame(datenum=res$datevec, res$result_array) #convert to dataframe
  
  obs_mod = merge(mod_df, obs) #merge observations and model results
  
  sum_runoff_mod_RiverFlow=NULL    #create variable (without content)
  sum_runoff_mod_DailyWaterSubbas=NULL 
  sum_runoff_obs=NULL
  sum_rainfall_obs=NULL
  
  # calculate rc_mod_RiverFlow = runoff for modelled "River_Flow"; only makes sense for head water catchments
  for (i in 1:length(subbas_id))
  {  
    subbas=subbas_id[i]
    
    relevant_columns = c(paste0(
      "River_Flow", 
      #"daily_water_subbasin",
      ifelse(length(subbas_id)==1, "", paste0("_X", subbas))),
      paste0("sub",subbas),
      paste0("rain_",subbas))
    
    concommittant_data = na.omit(obs_mod[, relevant_columns]) #select non-NA data only
    
    area = attr(res[[2]], "subbas_area")[i]
    if (subbas==4) #outlet - use rough areal mean of rainfall
      area = sum(attr(res[[2]], "subbas_area"))
    concommittant_data[,1:2] = concommittant_data[,1:2] * t_res / (area*1e6) * 1e3  #convert discharge from m?/s to mm
    sum_runoff_mod_RiverFlow[i]   = sum(concommittant_data[,1]) #mm
    sum_runoff_obs[i]   = sum(concommittant_data[,2]) #mm
    sum_rainfall_obs[i] = sum(concommittant_data[,3]) #mm
    
    if (subbas==4) #outlet - use rough areal mean of rainfall
    {  
      mean_rainfall_obs = apply(X = obs[,grepl(pattern = "rain_[0-9]*", x = names(obs) )], MAR=2, FUN=mean, na.rm=TRUE) #mean rainfall per timestep for each subbasin [mm]  
      sum_rainfall_obs[i] =  nrow(concommittant_data) * sum(mean_rainfall_obs * attr(res[[2]], "subbas_area") / sum(attr(res[[2]], "subbas_area"))) #area-weighted mean rainfall per timestep
    }
    
  }
  
  # calculate rc_mod_DailyWaterSubbas = runoff for modelled "daily_water_subbasin"
  for (i in 1:length(subbas_id))
  {  
    subbas=subbas_id[i]
    
    relevant_columns = c(paste0(
      #"River_Flow", 
      "daily_water_subbasin",
      ifelse(length(subbas_id)==1, "", paste0("_X", subbas))),
      paste0("sub",subbas),
      paste0("rain_",subbas))
    
    concommittant_data = na.omit(obs_mod[, relevant_columns]) #select non-NA data only
    
    area = attr(res[[2]], "subbas_area")[i]
    if (subbas==4) #outlet - use rough areal mean of rainfall
      area = sum(attr(res[[2]], "subbas_area"))
    concommittant_data[,1:2] = concommittant_data[,1:2] * t_res / (area*1e6) * 1e3  #convert discharge from m?/s to mm
    sum_runoff_mod_DailyWaterSubbas[i]   = sum(concommittant_data[,1]) #mm
    sum_runoff_obs[i]   = sum(concommittant_data[,2]) #mm
    sum_rainfall_obs[i] = sum(concommittant_data[,3]) #mm
    
    if (subbas==4) #outlet - use rough areal mean of rainfall
    {  
      mean_rainfall_obs = apply(X = obs[,grepl(pattern = "rain_[0-9]*", x = names(obs) )], MAR=2, FUN=mean, na.rm=TRUE) #mean rainfall per timestep for each subbasin [mm]  
      sum_rainfall_obs[i] =  nrow(concommittant_data) * sum(mean_rainfall_obs * attr(res[[2]], "subbas_area") / sum(attr(res[[2]], "subbas_area"))) #area-weighted mean rainfall per timestep
    }
    
  }
  
  # Label for plot "sum_runoff"
  #To fix "Error in plot.new(): figure margins too large", install calibrate package
  #install.packages(calibrate)
  #load the calibrate package
  #library(calibrate)
  
  windows()  
  # if plot doesn't work, clear plot history (broomstick symbol in Plot window)
  plot(sum_runoff_mod_RiverFlow, sum_runoff_obs, ylab="Total sum of runoff, observed [mm]", xlab="Total sum of runoff, modelled RiverFlow [mm]")
  abline(a=0, b=1)
  plot(sum_runoff_mod_DailyWaterSubbas, sum_runoff_obs, ylab="Total sum of runoff, observed [mm]", xlab="Total sum of runoff, modelled DailyWaterSubbas [mm]")
  abline(a=0, b=1)
  #textxy(sum_runoff_mod, sum_runoff_obs, sum_runoff_obs) #x-value, y-value, Label - funktioniert nur, wenn Daten Label/ID haben
  
  ##check if thread_dir exists to save plots, otherwise create it
  ## create folder to save the visualisation
  if ( dir.exists(thread_dir) == FALSE){
    dir.create(thread_dir)
  }
  
  savePlot(paste(thread_dir,"Sum_runoff","_",substring(thread_dir,1,nchar(thread_dir)-1),".png", sep = ""), "png")
  
  # Calculate runoff coefficients
  rc_obs = sum_runoff_obs / sum_rainfall_obs
  rc_mod_RiverFlow = sum_runoff_mod_RiverFlow / sum_rainfall_obs
  rc_mod_DailyWaterSubbas = sum_runoff_mod_DailyWaterSubbas / sum_rainfall_obs
  
  print(data.frame(name=attr(obs, "subbasin_names")[-1],rc_obs, rc_mod_RiverFlow,rc_mod_DailyWaterSubbas))
}

rc_data=data.frame(name=attr(obs, "subbasin_names")[-1],rc_obs, rc_mod_RiverFlow, rc_mod_DailyWaterSubbas)
rc_data=unique(rc_data) #remove duplicate rows
##########
# add clearer names
for (i in 1:nrow(rc_data)){
  rc_data[i,5] <- SubbasID_GaugeNumber[match(rc_data[i,1],SubbasID_GaugeNumber$Subbas_ID),"km.source"]
  rc_data[i,6] <- SubbasID_GaugeNumber[match(rc_data[i,1],SubbasID_GaugeNumber$Subbas_ID),"Index"]
}
rc_data <- rc_data[,c(1,5,6,2:4)]
names(rc_data)[c(1,2,3)] <- c("Subbasin ID","Info","Index")

# Display sorted runoff coefficients
rc_data[order(rc_data$Index),]

# little Quickfix to keep the Indexing for the next loop working
SubbasID_GaugeNumber <- SubbasID_GaugeNumber[SubbasID_GaugeNumber$Subbas_ID %in% subbas_id,]
####################### 
######WATER BALANCE#####  
########################
# change from original. Create loop visualize and save all the subbasins where observations are used
for ( i in SubbasID_GaugeNumber$Subbas_ID){
  
  subbas_id= i 
  if (TRUE) #  plot time series water
  {  
    res = read_wasa_results(ctrl_params,components_list=c(  # Choose here what to plot
      #"deep_gw_recharge",
      #"deep_gw_discharge",
      #"gw_loss",                                                      
      #"daily_qhorton",
      #"daily_subsurface_runoff",
      "daily_total_overlandflow",
      #"total_overlandflow",
      #"daily_actetranspiration",
      "River_Flow",
      #"daily_potetranspiration",
      "daily_water_subbasin",
      "water_subbasin"
    ), subbas_id=subbas_id) #read WASA simulation results
    dt = attr(res$result_array,"dt")
    
    
    #Observation Data
    
    ##discharge_obs_24.txt in WASA-Input Time_Series
    obs = read_observations(subbas_id=subbas_id, datevec=res$datevec, target_component=c("River_Flow","rain"), wasa_input_dir=ctrl_params$input_dir)
    
    ## if no observation data
    #obs=NULL
    
    source("plot_rainfall_runoff.R") #include plotting routine
    
    #convert to same units: m3/s
    cols_in_m3 = attr(res$result_array,"units") == "m3" | attr(res$result_array,"units") == "m3/d"
    res$result_array[, cols_in_m3] = res$result_array[, cols_in_m3] / dt / 3600
    
    area = attr(res[[2]], "subbas_area")  
    cols_in_mm = attr(res$result_array,"units") == "mm" | attr(res$result_array,"units") == "mm/d"
    res$result_array[, cols_in_mm] = res$result_array[, cols_in_mm] /1e3 * area * 1e6 / (24*3600)
    
    if (attr(res$result_array,"dt")==1)
      x=cbind(obs, mod=res$result_array[,c("River_Flow","water_subbasin",      "total_overlandflow")])[,] else
        x=cbind(obs, mod=res$result_array[,c("River_Flow","daily_water_subbasin","daily_total_overlandflow")])[,]
    
    #x=cbind(obs, mod=res$result_array[,c("River_Flow","daily_water_subbasin")])[,]
    #x=cbind(obs, mod=res$result_array[,c("River_Flow","water_subbasin")])[,]
    
    #x=cbind(obs, mod=res$result_array[,c("River_Flow")])[,]
    x$mod.River_Flow
    
    summary(res$result_array[,c("River_Flow","daily_water_subbasin")])
    
    windows()
    
    #Plot names if thread specified - Full time span
    plot_rainfall_runoff(x, xlim = NULL, ylim = NULL, x_col=1, subplot_assignment=c(0,2,1,2,2,2), s_colors = c("blue","cyan","red","magenta","black"), xlab="", ylab="",
                         main = SubbasID_GaugeNumber[match(subbas_id,SubbasID_GaugeNumber$Subbas_ID),"km.source"])
    
    ## create folder to save the visualisation
    if ( dir.exists(thread_dir) == FALSE){
      dir.create(thread_dir)
    }
    
    savePlot(paste(thread_dir,SubbasID_GaugeNumber[match(subbas_id,SubbasID_GaugeNumber$Subbas_ID),"km.source"],
                   "_",substring(thread_dir,1,nchar(thread_dir)-1), ".png", sep = ""), "png")
  }
} # end of for loop
#stop()

########

######## Proceed here  
Volume_error_DF <- data.frame( "ID" = c(), "Volume Error[mm]" = c(), "km.source" = c(), "Index" = c())

#read WASA simulation results # put the subbasins you want to look at in this vector
 #subbas_id = c(10,11,12) #Zone 1
# subbas_id=c(78,73,15,16,90,58,96,45) #Zone2
 subbas_id = c(1,2,3) #Zone3

if (save_plot) windows()
#plot water balance and runoff components
# set counter for indexing Volume Error dataframe
counter = 1
for (i in 1:length(subbas_id)) 
{ 
  res = read_wasa_results(ctrl_params,components_list=c(
    "gw_loss",                                                      
    #     "deep_gw_recharge",
    "deep_gw_discharge",
    
    #     #"daily_qhorton",
    "daily_subsurface_runoff",
    "daily_total_overlandflow",
    "daily_actetranspiration",
    "daily_water_subbasin"
    #     #"River_Flow",
    #     #"daily_potetranspiration",
    
    #"gw_discharge",
    
    #     "subsurface_runoff",
    #     "total_overlandflow",
    #     "actetranspiration",
    #     "water_subbasin"
    
  ), subbas_id=subbas_id[i]) #read WASA simulation results
  dt = attr(res$result_array,"dt")
  
  dimnames(  res$result_array)[[2]] = sub(pattern="daily_|deep_", repl="", dimnames(  res$result_array)[[2]]) #work with daily data and hourly data with the same names
  
  obs = read_observations(subbas_id=subbas_id[i], datevec=res$datevec, target_component=c("River_Flow","rain"), wasa_input_dir=ctrl_params$input_dir)
  
  
  #convert to same units: mm
  area = attr(res[[2]], "subbas_area") #subbasin area[km²]
  
  cols_in_m3 = attr(res$result_array,"units") == "m3" | attr(res$result_array,"units") == "m3/d"
  res$result_array[, cols_in_m3] = res$result_array[, cols_in_m3] / (area*1e6) * 1e3
  
  cols_in_m3s = attr(res$result_array,"units") == "m3/s" 
  res$result_array[, cols_in_m3s] = res$result_array[, cols_in_m3s]* (dt*3600) / (area*1e6) * 1e3
  
  mod_df=data.frame(datenum=res$datevec, res$result_array) #convert to dataframe
  obs_mod = merge(mod_df, obs) #merge observations and model results
  
  totals=apply(obs_mod[,-1], 2, sum) #sum up total amounts
  names(totals) = gsub(names(totals), pattern="rain_.*", repl="rain") #rename rain column
  
  #water balance
  # groundwater storage
  tt=read.table(paste0(ctrl_params$output_dir, "gw_storage.stat"), header=TRUE, skip=1)
  tt=tt[tt$Subbasin %in% subbas_id[i],]
  tt$stor_m3 = tt[,3]*1e3 * tt[,4]   #compute storage [m³]
  t2 = aggregate(tt[,-(1:3)], by=list(subbas_id=tt$Subbasin), FUN=sum)
  gw_storage=data.frame(subbas_id=t2$subbas_id, end=t2$stor_m3/t2[,2] /1e3)
  
  tt=read.table(paste0(ctrl_params$output_dir, "gw_storage.stat_start"), header=TRUE, skip=1)
  tt=tt[tt$Subbasin %in% subbas_id[i],]
  tt$stor_m3 = tt[,3]*1e3 * tt[,4]   #compute storage [m³]
  t2 = aggregate(tt[,-(1:3)], by=list(subbas_id=tt$Subbasin), FUN=sum)
  gw_storage$start= t2$stor_m3/t2[,2] /1e3
  gw_storage = gw_storage[gw_storage$subbas_id %in% subbas_id,]
  gw_storage$balance = gw_storage$end - gw_storage$start
  
  # #interception
  tt=read.table(paste0(ctrl_params$output_dir, "intercept_storage.stat"), header=TRUE, skip=1)
  tt=tt[tt$Subbasin %in% subbas_id[i],]
  tt$stor_m3 = tt[,5]*1e3 * tt[,6]   #compute storage [m³]
  t2 = aggregate(tt[,-(1:5)], by=list(subbas_id=tt$Subbasin), FUN=sum)
  intercept_storage=data.frame(subbas_id=t2$subbas_id, end=t2$stor_m3/t2[,2] /1e3)
  
  tt=read.table(paste0(ctrl_params$output_dir, "intercept_storage.stat_start"), header=TRUE, skip=1)
  tt=tt[tt$Subbasin %in% subbas_id[i],]
  tt$stor_m3 = tt[,5]*1e3 * tt[,6]   #compute storage [m³]
  t2 = aggregate(tt[,-(1:5)], by=list(subbas_id=tt$Subbasin), FUN=sum)
  intercept_storage$start =  t2$stor_m3/t2[,2] /1e3
  intercept_storage$balance = intercept_storage$end - intercept_storage$start
  
  # #river storage ### Hier ist der Fehler, apparently not needed
  # tt=read.table(paste0(ctrl_params$output_dir, "river_storage.stat"), header=FALSE, skip=2) ## This change is due to the different file format when using Unit Hydrograph
  # names(tt)[1] = "Subbasin"
  # tt = tt[tt$Subbasin %in% subbas_id[i],]
  # river_storage=data.frame(subbas_id=tt$Subbasin, end=tt[,2]/(area*1e6) * 1e3)
  # 
  # tt=read.table(paste0(ctrl_params$output_dir, "river_storage.stat_start"), header=TRUE, skip=1, check.names = FALSE)
  # tt = tt[tt$Subbasin %in% subbas_id[i],]
  # river_storage$start = tt[,2]/(area*1e6) * 1e3
  # river_storage$balance = river_storage$end - river_storage$start
  
  #soil_moisture
  tt=read.table(paste0(ctrl_params$output_dir, "soil_moisture.stat"), header=TRUE, skip=1)
  tt=tt[tt$Subbasin %in% subbas_id[i],]  
  tt$stor_m3 = tt[,6]*1e3 * tt[,7]   #compute storage [m³]
  t2 = aggregate(tt[,-(1:6)], by=list(subbas_id=tt$Subbasin), FUN=sum)
  soil_moisture=data.frame(subbas_id=t2$subbas_id, end=t2$stor_m3/t2[,2] /1e3)
  
  tt=read.table(paste0(ctrl_params$output_dir, "soil_moisture.stat_start"), header=TRUE, skip=1)
  tt=tt[tt$Subbasin %in% subbas_id[i],]
  tt$stor_m3 = tt[,6]*1e3 * tt[,7]   #compute storage [m³]
  t2 = aggregate(tt[,-(1:6)], by=list(subbas_id=tt$Subbasin), FUN=sum)
  soil_moisture$start =  t2$stor_m3/t2[,2] /1e3
  soil_moisture = soil_moisture[soil_moisture$subbas_id %in% subbas_id[i],]
  soil_moisture$balance = soil_moisture$end - soil_moisture$start
  
  #lake
  # lakefile=paste0(ctrl_params$output_dir, "lake_volume.stat")
  # if (!file.exists(lakefile))
  #   lake_volume=data.frame(start=0, end=0, balance=0) else
  # {    
  #   tt=read.table(lakefile, header=TRUE, skip=1)
  #   tt = tt[tt$Subbasin %in% subbas_id,]  
  #   if (nrow(tt) ==0) t2=data.frame(subbas_id=subbas_id, storage=0) else
  #   t2 = aggregate(tt[,-(1:2), drop=FALSE], by=list(subbas_id=tt$Subbasin), FUN=sum)
  #   lake_volume=data.frame(subbas_id=t2$subbas_id, end=t2[,2]/(area*1e6) * 1e3)  
  #   
  #   tt=read.table(paste0(ctrl_params$output_dir, "lake_volume.stat_start"), header=TRUE, skip=1)
  #   tt = tt[tt$Subbasin %in% subbas_id,]  
  #   if (nrow(tt) ==0) t2=data.frame(subbas_id=subbas_id, storage=0) else
  #   t2 = aggregate(tt[,-(1:2), drop=FALSE], by=list(subbas_id=tt$Subbasin), FUN=sum)
  #   lake_volume$start = t2[,2]/(area*1e6) * 1e3
  #   lake_volume$balance = lake_volume$end - lake_volume$start
  # }
  
  par(mfcol=c(1,2))
  #plot balance
  plotdata=cbind(input=c(totals["rain"], ET=0, gw_loss=0),
                 output=c(runoff=totals["water_subbasin"], ET=totals["actetranspiration"], gw_loss=totals["gw_loss"])
  )
  row.names(plotdata)=c("rain_runoff", "ET", "gw_loss")
  balances = c(gw_storage=gw_storage$balance, intercept_storage=intercept_storage$balance, soil_storage=soil_moisture$balance) #, river_storage=river_storage$balance) , lake_storage=lake_volume$balance
  storages=data.frame(depleted = pmax(0,-balances), filled = pmax(0,balances), row.names=names(balances))
  
  plotdata = rbind(plotdata, 
                   as.matrix(storages)
  )
  plotdata = plotdata[nrow(plotdata):1,] #reverse order
  
  barplot(height=plotdata, legend.text=row.names(plotdata), col=rainbow(nrow(plotdata)),
          main = SubbasID_GaugeNumber[match(subbas_id[i],SubbasID_GaugeNumber$Subbas_ID),"km.source"],
          ylim=c(0,max(apply(plotdata,2,sum)*1.7)), args.legend=list(x="top"), ylab="[mm]")
  print(paste0("Volume error [mm]: ",diff(apply(plotdata,2,sum))))
  
  Volume_error_DF[counter,"ID"] <- subbas_id[i]
  Volume_error_DF[counter,"Volume Error [mm]"] <- diff(apply(plotdata,2,sum))
  Volume_error_DF[counter,"km.source"] <- SubbasID_GaugeNumber[match(subbas_id[i],SubbasID_GaugeNumber$Subbas_ID),"km.source"]
  Volume_error_DF[counter,"Index"] <- SubbasID_GaugeNumber[match(subbas_id[i],SubbasID_GaugeNumber$Subbas_ID),"Index"]
  
  #plot runoff components
  plotdata=totals[c("gw_discharge", "subsurface_runoff", "total_overlandflow")]
  plotdata["subsurface_runoff"] = plotdata["subsurface_runoff"] - plotdata["gw_discharge"]
  names(plotdata) =c("groundwater","interflow", "surface")
  
  vol_error=abs(totals["water_subbasin"] - sum(plotdata))/totals["water_subbasin"]
  #if (vol_error > 0.01)
  #stop(paste0("large volume error in runoff components: ", vol_error))
  plotdata = cbind(components = plotdata
                   #                 total=c(totals["daily_water_subbasin"],0,0)
  )
  
  barplot(height=plotdata, legend.text=row.names(plotdata), col=rainbow(nrow(plotdata)), ylab="[mm]",
          main=paste0("runoff subbasin ",main = SubbasID_GaugeNumber[match(subbas_id[i],SubbasID_GaugeNumber$Subbas_ID),"km.source"]),
          ylim=c(0,sum(plotdata)*1.4), args.legend=list(x="top"))
  points(1,totals["water_subbasin"], pch="*", cex=2)
  
  
  ## create folder to save the visualisation
  if ( dir.exists(thread_dir) == FALSE){
    dir.create(thread_dir)
  }
  
  if (save_plot) savePlot(filename = paste(thread_dir,"waterBalance_",
                                           SubbasID_GaugeNumber[match(subbas_id[i],SubbasID_GaugeNumber$Subbas_ID),"km.source"]
                                           ,"_",substring(thread_dir,1,nchar(thread_dir)-1),".png",sep = ""), type="png")
  counter = counter + 1
}

####################################
# This seems to be just for sediment
# ############
# if (save_plot) windows()
# #plot modelled sediment balance 
# for (subbas_id in 31:31)
#   #subbas_id=31
# {  
#   res = read_wasa_results(ctrl_params,components_list=c(
#     #     #"deep_gw_recharge",
#     #"deep_gw_discharge",
#     #"gw_discharge",
#     #"gw_loss",                                                      
#     
#     #     #"daily_qhorton",
#     #     "daily_subsurface_runoff",
#     #     "daily_total_overlandflow",
#     #     "daily_actetranspiration",
#     #     #"River_Flow",
#     #     #"daily_potetranspiration",
#     #     "daily_water_subbasin"
#     #     
#     #"subsurface_runoff",
#     #"total_overlandflow",
#     #"actetranspiration",
#     #"water_subbasin"
#     "River_Sediment_total",
#     "daily_sediment_production"
#     
#   ), subbas_id=subbas_id) #read WASA simulation results
#   dt = attr(res$result_array,"dt")
#   
#   obs=NULL
#   #obs = read_observations(subbas_id=subbas_id, datevec=res$datevec, target_component=c("River_Flow","rain"), wasa_input_dir=ctrl_params$input_dir)
#   
#   
# #   #convert to same units: t/ [dt]
# #   #area = attr(res[[2]], "subbas_area") #subbasin area[km²]
# #   
# #   cols_in_m3 = attr(res$result_array,"units") == "m3" | attr(res$result_array,"units") == "m3/d"
# #   res$result_array[, cols_in_m3] = res$result_array[, cols_in_m3] / (area*1e6) * 1e3
# #   
# #   cols_in_m3s = attr(res$result_array,"units") == "m3/s" 
# #   res$result_array[, cols_in_m3s] = res$result_array[, cols_in_m3s]* (dt*3600) / (area*1e6) * 1e3
# #   
#   mod_df=data.frame(datenum=res$datevec, res$result_array) #convert to dataframe
#   #obs_mod = merge(mod_df, obs) #merge observations and model results
#   obs_mod = mod_df
#   
#   totals=apply(obs_mod[,-1], 2, sum) #sum up total amounts
#   names(totals) = c("sediment yield", "gross erosion")
#   
#   #sediment balance
#   #riverbed storage
#   tt=read.table(paste0(ctrl_params$output_dir, "sediment_storage.stat"), header=TRUE, skip=1, sep="\t")
#   tt=tt[tt$Subbasin %in% subbas_id,]
#   t2 = aggregate(tt[,3], by=list(subbas_id=tt$Subbasin), FUN=sum)
#   sed_storage=data.frame(subbas_id=t2$subbas_id, end=t2$x)
#   
#   tt=read.table(paste0(ctrl_params$output_dir, "sediment_storage.stat_start"), header=TRUE, skip=1, sep="\t")
#   tt=tt[tt$Subbasin %in% subbas_id,]
#   t2 = aggregate(tt[,3], by=list(subbas_id=tt$Subbasin), FUN=sum)
#   sed_storage$start= t2$x
#   sed_storage = sed_storage[sed_storage$subbas_id %in% subbas_id,]
#   sed_storage$balance = sed_storage$end - sed_storage$start
#   
#   #storage in suspension
#   tt=read.table(paste0(ctrl_params$output_dir, "susp_sediment_storage.stat"), header=TRUE, skip=1, sep="\t")
#   tt=tt[tt$Subbasin %in% subbas_id,]
#   t2 = aggregate(tt[,3], by=list(subbas_id=tt$Subbasin), FUN=sum)
#   susp_sed_storage=data.frame(subbas_id=t2$subbas_id, end=t2$x)
#   
#   tt=read.table(paste0(ctrl_params$output_dir, "susp_sediment_storage.stat_start"), header=TRUE, skip=1, sep="\t")
#   tt=tt[tt$Subbasin %in% subbas_id,]
#   t2 = aggregate(tt[,3], by=list(subbas_id=tt$Subbasin), FUN=sum)
#   susp_sed_storage$start= t2$x
#   susp_sed_storage = susp_sed_storage[susp_sed_storage$subbas_id %in% subbas_id,]
#   susp_sed_storage$balance = susp_sed_storage$end - susp_sed_storage$start
#   
#  
#   
#   
#   #par(mfcol=c(1,2))
#   #plot balance
#   plotdata=cbind(input=c(totals["gross erosion"]),
#                  output=c(yield=totals["sediment yield"])
#   )
#   row.names(plotdata)=c("erosion/yield")
#   balances = c(sed_storage=sed_storage$balance, susp_sed_storage=susp_sed_storage$balance)
#   storages=data.frame(depleted = pmax(0,-balances), filled = pmax(0,balances), row.names=names(balances))
#   
#   plotdata = rbind(plotdata, 
#                    as.matrix(storages)
#   )
#   plotdata = plotdata[nrow(plotdata):1,] #reverse order
#   
#   barplot(height=plotdata, legend.text=row.names(plotdata), col=rainbow(nrow(plotdata)), main=subbas_id,
#           ylim=c(0,max(apply(plotdata,2,sum)*1.7)), args.legend=list(x="top"), ylab="[t]")
#   print(paste0("Mass error [t]: ",diff(apply(plotdata,2,sum))))
#   
#  
#   
#   if (save_plot) savePlot(filename=subbas_id, type="png")
#   
# }
# 
# 
# if (save_plot) windows()
# if (FALSE) #compare modelled and measured yields
# {  
#   subbas_id=1:6  #select subbasins to read
# 
#   components_list="River_Sediment_total"
#   xx = read_wasa_results(ctrl_params, components_list=components_list,
#    subbas_id=subbas_id)  #the strange structure is due to compatibility wiht RHydro package
# 
#   d.wasa.out <- t(xx[[2]]) #modelled components
#   dim(d.wasa.out)=c(1,dim(d.wasa.out))    #reshape array to match style of WASIM-package
#   dimnames(d.wasa.out)[2]= dimnames(xx[[2]])[2] #keep names
#   data.types=data.frame(X1.29=nchar(components_list),beschreibung_en=components_list, prefix=components_list, has_stat=is.null(components_list))  #match style of WASIM-package
#   
#   
#   datevec= xx[[1]]       #date information
#   
#   d.meas.all = read_observations(subbas_id=subbas_id, datevec=datevec, target_component=c("River_Sediment_total"), wasa_input_dir=ctrl_params$input_dir)
# 
#   if (all(is.na(d.meas.all[,-1]))) stop("no observation data in timespan. Check observed sediment time series") 
#   if (ncol(d.meas.all) < length(subbas_id)+1) stop("Couldn't find all subbasin data (found ", paste(names(d.meas.all)[-1], collapse=", "), "). Check discharge_obs_*.txt") 
#   
#   start_date=min(datevec)    #get start and end of modelled time series
#   end_date=max(datevec)
# 
#   #trim observed and modelled data to desired time period
#   d.meas.all=d.meas.all[d.meas.all$datenum>=start_date & d.meas.all$datenum<=end_date,]
#   
#   d.wasa.out = d.wasa.out[,,datevec>=start_date & datevec<=end_date, drop=FALSE]
#   if (length(d.wasa.out)==0 || length(d.wasa.out[1,1,]) == 0) stop("compute_goodness_measures: modelled data doesn't cover required timespan")
#   
#   datevec = datevec[datevec>=start_date & datevec<=end_date]
# 
#   array_offset_sed=0  
#   
#   collected=data.frame()
#   for (subbas_counter in 1:length(subbas_id))
#   {
#       #    browser()
#       
#       #assign sediment fluxes
#       q_sed_mod = d.wasa.out[1, array_offset_sed + subbas_counter, ]
#       q_sed_meas = d.meas.all[, paste0("monthly_yield_sub", subbas_id[subbas_counter])]    
#       
#       if (any(grepl(pattern="monthly_yield", x=names(d.meas.all)))) #if observations are monthly sediment yield
#       {
#         compiled=data.frame(datevec=datevec, q_sed_mod = q_sed_mod, q_sed_meas=q_sed_meas)
#         no_nas=range(which(!is.na(q_sed_mod+q_sed_meas))) #find first and last no-na record
#         
#         compiled$month_code = (as.POSIXlt(compiled$datevec)[["year"]]-100)*100 + (as.POSIXlt(compiled$datevec)[["mon"]]+1)
#         compiled=compiled[compiled$month_code >= compiled$month_code[no_nas[1]] &
#                             compiled$month_code <= compiled$month_code[no_nas[2]], ] 
#         
#         tt=aggregate(x=compiled[,-c(1,4)], by=list(compiled$month_code), FUN=sum, na.rm=TRUE)[,-1]
#         q_sed_mod=tt$q_sed_mod
#         q_sed_meas=tt$q_sed_meas
#       }  
#       collected=rbind(collected, data.frame(subbas=subbas_id[subbas_counter], obs=q_sed_meas, mod=q_sed_mod))
#   }
#   palette(rainbow(6))
#   plot(collected$mod, collected$obs, col=collected$subbas, pch = 20) 
#   legend(x="topleft", legend=attr(d.meas.all, "subbasin_names")[-1], col=unique(collected$subbas), pch = 20)
#   abline(a=0, b=1)
#   
#   totals=aggregate(collected[,-1], by=list(subbas=collected$subbas), FUN = sum)
#   totals$names=attr(d.meas.all, "subbasin_names")[-1]
#   
#   plot(totals$mod, totals$obs, col=1:nrow(totals), pch = 20) 
#   legend(x="topleft", legend=attr(d.meas.all, "subbasin_names")[-1], col=1:nrow(totals), pch = 20)
#   abline(a=0, b=1)
#   print(totals)
# 
#   write.table(collected, file=paste0(run_dir,"/sed_yield_obs_mod.txt"), sep="\t", row.names=FALSE, quote=FALSE)            
#   if (save_plot) savePlot(filename=paste0(run_dir,"/sed_yield"), type="png")
# }
# 
# 
# subbas_id=1
# if (TRUE) # single subbas, plot time series sediment
# {  
#   res = read_wasa_results(ctrl_params,components_list=c(
#     "River_Sediment_total",
#     "daily_sediment_production",
#     "daily_total_overlandflow",
#     
#     "River_Flow",
#         "daily_water_subbasin"
#     #"water_subbasin"
#   ), subbas_id=subbas_id) #read WASA simulation results
#   dt = attr(res$result_array,"dt")
#   obs = read_observations(subbas_id=subbas_id, datevec=res$datevec, target_component=c("River_Flow","rain"), wasa_input_dir=ctrl_params$input_dir)
#   
#   source("plot_rainfall_runoff.R") #include plotting routine
#   
#   
#   #convert to same units: m?/s
#   cols_in_m3 = attr(res$result_array,"units") == "m3" | attr(res$result_array,"units") == "m3/d"
#   res$result_array[, cols_in_m3] = res$result_array[, cols_in_m3] / dt / 3600
#   
#   area = attr(res[[2]], "subbas_area")  
#   cols_in_mm = attr(res$result_array,"units") == "mm" | attr(res$result_array,"units") == "mm/d"
#   res$result_array[, cols_in_mm] = res$result_array[, cols_in_mm] /1e3 * area * 1e6 / (24*3600)
#   
# 
#   x=cbind(obs, mod=res$result_array[,c("River_Flow","daily_water_subbasin","daily_total_overlandflow",
#                                        "River_Sediment_total","daily_sediment_production")])[,]
# 
#   
#   windows()
#   playwith(
#     plot_rainfall_runoff(x = x, xlim = NULL, ylim = NULL, subplot_assignment=c(0,2,1,2,2,2,3,3), s_colors = c("blue", "blue","red","magenta","black","red","magenta"), xlab="", ylab="", main = paste0(run_dir,"/",thread_dir,"; subbas:", subbas_id ))
#   )
# }