#WASA calibration using PSO / DDS
#to be called like:
#nohup R --vanilla -f calibrate_main_dds.R --slave > console_output.txt &
# Copyright (C) 2020 Dr. Till Francke


    param_ranges=rbind(         #define parameter ranges
    log_gw_delay_f=c(-2,log10(100)),
    log_soildepth_f=c(log10(0.1),log10(100)),
    log_kf_bedrock_f=c(log10(1e-5),log(1e2)),
    riverdepth_f=c(1e-1,20),
    log_kf_scale_f=c(log10(1e-7),log10(1)),         #5 
    log_ksat_factor=c(log10(1e-3),log10(10)),
    f_wind=c(1,5),
    f_gw_direct=c(0.4,1),
    LAI_f =c(0.1,5),
    rootd_f = c(0.5,5),
    stomr_f = c(0.5,5)
#    albedo_f = c(0.5,4)
    

#    riv_depth_f=c(0.01,30),
#     riv_width_f=c(0.001,10),            #8
#     riv_side_ratio_f=c(0.1,10),
#     riv_bottom_width_of_floodplain_f=c(0.1,40),     #
#     riv_side_ratio_floodplains_f=c(0.1,20),
#     riv_channel_slope_f=c(0.1,20),                  #
#    riv_length_f=c(0.3,3),
#    riv_manningn_f=c(0.1,4),
#     riv_manningn_floodplain_f=c(0.1,10),            #
#     riv_baseflowalphafactor_f=c(0.1,10),            #
     # riv_Muskingum_X_f=c(0.05,1.5),     #SWAT-theo: 0..0.5 (not _f!)
     # riv_Muskingum_K_f=c(0.005,10),                     
     # log_riv_Ksat_f=c(log(1e-8),log(20))
    )

starting.values=rbind(               #initial estimates
    log_gw_delay_f=c(log10(1)),
    log_soildepth_f=c(log10(1)),
    log_kf_bedrock_f=c(log10(1)),
    riverdepth_f=c(1),
    log_kf_scale_f=c(log10(1)),
    log_ksat_factor=c(log10(1)),
    f_wind=c(1),
    f_gw_direct = c(0.8),
    LAI_f =c(1),
    rootd_f = c(1),
    stomr_f = c(1)
#   albedo_f = c(1)

#     riv_depth_f=1,
#     riv_width_f=1,
#     riv_side_ratio_f=1,
#     riv_bottom_width_of_floodplain_f=1,
#     riv_side_ratio_floodplains_f=1,
#     riv_channel_slope_f=1,
#     riv_length_f=1,
#     riv_manningn_f=1,
# #     riv_manningn_floodplain_f=1,
# #     riv_baseflowalphafactor_f=1,
#     riv_Muskingum_X_f=1,
#     riv_Muskingum_K_f=1,                            
#     log_riv_Ksat_f=log10(0.1)
    )
    
max_number_function_calls = 2000
    
if (file.exists("init_estimates.txt")) #load initial estimates, if present
{  
  starting.values = t(read.table(file="init_estimates.txt", header=TRUE))
  max_number_function_calls =  max_number_function_calls + 2000
}
library(ppso)
source("optim_wrapper.R") #include objective function
    
#test    
#parms=starting.values[,1]
#res=optim_wrapper(parms)

#full call pdds = parallelisiert für Cluster
    res <- optim_pdds_robust(objective_function=optim_wrapper, number_of_particles=15, number_of_parameters=NROW(param_ranges), parameter_bounds=param_ranges, initial_estimates=starting.values, lhc_init=TRUE, part_xchange=3,
                                logfile="dds.log",projectfile="dds.pro", load_projectfile="try", break_file="stop.pso", nslaves=-1, max_wait_iterations=1000, max_number_function_calls=max_number_function_calls, tryCall=TRUE, execution_timeout=10)
    # dds linear, für meinen Laptop
   # res <- optim_dds(objective_function=optim_wrapper, number_of_particles=1, number_of_parameters=NROW(param_ranges), parameter_bounds=param_ranges, initial_estimates=starting.values, lhc_init=TRUE, part_xchange=3,
  #                              logfile="dds.log",projectfile="dds.pro", load_projectfile="try", break_file="stop.pso", max_wait_iterations=1000, max_number_function_calls=max_number_function_calls, tryCall=TRUE)
    
 print(res)






