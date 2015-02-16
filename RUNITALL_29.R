################################################
# Spatially-explicit aquatic plant growth      #
# Floating plant - submerged plant competition # 
# Now compatible with new LIST structure       #
#                                              #
# By: Michael J. McCann                        #
# Major revision: July 2014                    #
################################################
# 
# CURRENT SETUP:
# Multiple species of duckweed growing - Each species is on their own 2-d grid (matrix)
# Positions of plants based on X,Y coordinates in a matrix   
# Includes an overwintering step, where there is a large die-off of individuals 
#
# TO DO:
#
# Print parameter value labels on outputs (.gif or .jpg files)
# Can I get saveHTML (package animation) outputs to save well 
# Figure out why I'm using saveHTML and not saveGIF() in package animation 
# Modify reproduction so it gives up after a max distance of looking for a place to reproduce (so new plants aren't thrown too far)
#
# Example for naming files/plots with variable values 
# x=1
# paste(c("x = ", x), collapse = "")
#
# ISSUES & PROBLEMS: 
# 
########################################################################################################

########################################
# load workspace for de-bugging
# 
#
########################################

# imports parameter  values for all simulations
parameters <- read.csv("input_wind_size_and_shape.csv")

# only take simulations with wind scenario A1
parameters <- subset(parameters, parameters$scenario == "A1")

# subset smaller chunks 
#parameters <- parameters[1:12,]
#parameters <- parameters[13:600,]
#parameters <- parameters[601:1200,]
#parameters <- parameters[1201:1800,]
#parameters <- parameters[2401:3600,]
#parameters <- parameters[3601:4800,]
#parameters <- parameters[4801:6000,]
#parameters <- parameters[6001:7200,]

# Check for errors in the input file 
# source(file=paste(getwd(),"/FUNCTIONS/WARNING.R",sep=""),chdir=TRUE)
# WARNING(parameters)

# add blank columns to parameters for each of the results 
parameters$SAV_end_yr01 <- rep(NA, nrow(parameters)) 
parameters$SAV_end_yr02 <- rep(NA, nrow(parameters))
parameters$SAV_end_yr03 <- rep(NA, nrow(parameters))
parameters$SAV_end_yr04 <- rep(NA, nrow(parameters))
parameters$FP_end_yr01 <- rep(NA, nrow(parameters))
parameters$FP_end_yr02 <- rep(NA, nrow(parameters))
parameters$FP_end_yr03 <- rep(NA, nrow(parameters))
parameters$FP_end_yr04 <- rep(NA, nrow(parameters))
parameters$TOTALN_end_yr01 <- rep(NA, nrow(parameters))
parameters$TOTALN_end_yr02 <- rep(NA, nrow(parameters))
parameters$TOTALN_end_yr03 <- rep(NA, nrow(parameters))
parameters$TOTALN_end_yr04 <- rep(NA, nrow(parameters))

# load the packages you need 
require(foreach)
require(doSNOW)
require(R.utils) # package for sourceDirectory() - loeding all the functions in a directory 
 
# make the correct number of clusters - the first argument will change depending on the machine / set-up 
cl <- makeCluster(4,"SOCK") 

# load all your functions
sourceDirectory(path=paste(getwd(),"/FUNCTIONS29",sep=""), recursive=FALSE, modifiedOnly=FALSE) 

#  assigns the functions to the global environments of each node
clusterExport(cl, c("BLANK29", "GROW_SAV29", "GROW_FP29", 
                    "INPUT29","MOVE29","OUTPUT_29",
                    "OVERWINTER29","SPECIES29","START29",
                    "STEP29","UPTAKE_N29","UPTAKE_P29","WIND29"))

registerDoSNOW(cl) # registers the SNOW parallel backend w/ foreach package 
getDoParWorkers() # returns the # of workers - this should match the # of cores on your machine (or # cores - 1)

RESULT <- foreach (i=1:nrow(parameters), .combine=rbind, .errorhandling='pass') %dopar% { # loop through all of your simulations - User needs to specify the max # of simulations (rows of parameters) in .csv
  
  
  # I am not sure if this is neccasary. Can I just use i as the input to the functions below?
  simulnumb <- i # assigns the simulation # from the for loop - will be used as an input to INPUT() to read the right row of .csv
  
  INPUT29(simulnumb) # reads the .csv file of parameter values and assigns them to the global environment 
  
  specieslist <- SPECIES29() # function that builds the list of species-specific parameters that is used in STEPX()  
  
  # define a couple of things in the environment that get used in STEPX() and OUTPUT()
  timesteps <- years*(days+1) # this will need to change b/c of overwintering 
  winters <- (days+1) * seq(from=1, to=years, by=1) # ID timesteps that are winters - used in STEPX()
  mix_days <- seq(mix_freq+1,timesteps,mix_freq)
  
  # generate blank list for total timesteps
  LIST<-NULL
  for (i in 1:timesteps){ 
    LIST[[i]] <- BLANK29()
  }
  
  # initialize first time step
  LIST[[1]] <- START29(LIST[[1]])  
  
  # for loop - STEP() to the entire LIST
  today <- LIST[[1]]
  
  for (t in 1:timesteps){
    
    tomorrow <- STEP29(today,t)
    
    LIST[[t+1]] <- tomorrow
    
    today <- tomorrow
    
    ##################################
    # Plot as you go (slows it down) #
    ##################################
    # require(raster)
    
    # make raster layers 
    # LANDrast<-raster(LAND)
    # SAV<-raster(LIST[[t]]$SAV)
    # for (y in 1:numbFPspecies){
    #    assign(paste("FP0",y,sep=""),raster(LIST[[t]]$FP[[y]]))
    # }
    # FPtotal<-raster(LIST[[t]]$FPtotal)
    # nitrogen<-raster(LIST[[t]]$TOTALN/1000)
    #  
    # stack raster layers 
    # I need a smarter way to make this variable length 
    # if (numbFPspecies == 4){
    #  all_layers <- stack(LANDrast,SAV,nitrogen,FPtotal,FP01,FP02,FP03,FP04)
    # }
    # if (numbFPspecies == 3){
    #   all_layers <- stack(LANDrast,SAV,nitrogen,FPtotal,FP01,FP02,FP03)
    # }
    # if (numbFPspecies == 2){
    #   all_layers <- stack(LANDrast,SAV,nitrogen,FPtotal,FP01,FP02)
    # }
    # if (numbFPspecies == 1){
    #   all_layers <- stack(LANDrast,SAV,nitrogen,FPtotal,FP01)
    # }
    # 
    # name raster layers 
    # names(all_layers)[1] <- "LAND"
    # names(all_layers)[2] <- "SAV"
    # names(all_layers)[3] <- "nitrogen"
    # names(all_layers)[4] <- "FPtotal"
    # for (y in 1:numbFPspecies){
    #  names(all_layers)[y+4] <- paste("FP0",y,sep="")
    # }
    # 
    # plot raster layers 
    # plot(all_layers,main=t)
    
    # print timestep to console - SLOW!
    # print(t)
  }    
  
  # generates graphs
  # if you want .html animation you must specify animate=TRUE
  # set "FP regime" threshold here  
  OUTPUT_29(shapshots=FALSE,timeseries=FALSE) 
  
  # When not using foreach() loop: 
  # RESULTS[simulnumb,1] <- propyears_avgFPcover_abovethreshold # assign the current simulations results to the correct spot
  # RESULTS[simulnumb,2] <- propyears_prop_daysFP_abovehalf # assign the current simulations results to the correct spot
  # RESULTS[simulnumb,3] <- avg_avg_FPcover # assign the current simulations results to the correct spot  
  
  # When not using foreach() loop (alternative):
  # parameters$propyears_avgFPcover_abovethreshold[simulnumb] <- propyears_avgFPcover_abovethreshold
  # parameters$propyears_prop_daysFP_abovehalf[simulnumb] <- propyears_prop_daysFP_abovehalf
  # parameters$avg_avg_FPcover[simulnumb] <- avg_avg_FPcover
    
  # these results are produced by OUTPUT()
  # stick all of the results you want out in a vector together 
  # I'm not sure what this is actually doing, 
  # since I am not assigning this vector to anything
  # BUT, if I take it out, writing "output.csv" does not work
  # Error in `colnames<-`(`*tmp*`, value = c("simulnumb", "propyears_avgFPcover_abovethreshold",  : 
  # length of 'dimnames' [2] not equal to array extent
  c(simulnumb,
    SAV_end_yr01,
    SAV_end_yr02,
    SAV_end_yr03,
    SAV_end_yr04,
    FP_end_yr01,
    FP_end_yr02,
    FP_end_yr03,
    FP_end_yr04,
    TOTALN_end_yr01,
    TOTALN_end_yr02,
    TOTALN_end_yr03,
    TOTALN_end_yr04
  )
    
}

# stop the cluster 
stopCluster(cl)

# name columns of RESULT  
colnames(RESULT) <- c("simulnumb",
                      "SAV_end_yr01",
                      "SAV_end_yr02",
                      "SAV_end_yr03",
                      "SAV_end_yr04",
                      "FP_end_yr01",
                      "FP_end_yr02",
                      "FP_end_yr03",
                      "FP_end_yr04",
                      "TOTALN_end_yr01",
                      "TOTALN_end_yr02",
                      "TOTALN_end_yr03",
                      "TOTALN_end_yr04"
                      )

# convert to a data frame 
RESULT <- as.data.frame(RESULT)

# Order the RESULT by simulation number 
# create a vector of simulnumb in increasing order 
order.simulnumb <- order(RESULT$simulnumb)

# use that vector to order the RESULT data frame 
RESULT <- RESULT[order.simulnumb,] 

# add the columns of the RESULT data frame to the original parameters data frame 
parameters$SAV_end_yr01 <- RESULT[,2]
parameters$SAV_end_yr02 <- RESULT[,3]
parameters$SAV_end_yr03 <- RESULT[,4]
parameters$SAV_end_yr04 <- RESULT[,5]
parameters$FP_end_yr01 <- RESULT[,6]
parameters$FP_end_yr02 <- RESULT[,7]
parameters$FP_end_yr03 <- RESULT[,8]
parameters$FP_end_yr04 <- RESULT[,9]
parameters$TOTALN_end_yr01 <- RESULT[,10]
parameters$TOTALN_end_yr02 <- RESULT[,11]
parameters$TOTALN_end_yr03 <- RESULT[,12]
parameters$TOTALN_end_yr04 <- RESULT[,13]

# write parameters with RESULT appended to a .csv 
write.csv(parameters,"output_wind_size_and_shapeF.csv",row.names=F)
#write.csv(parameters,"output_wind_size_and_shapeG.csv",row.names=F)
#write.csv(parameters,"output_wind_size_and_shapeH.csv",row.names=F)
#write.csv(parameters,"output_wind_size_and_shapeI.csv",row.names=F)
#write.csv(parameters,"output_wind_size_and_shapeJ.csv",row.names=F)
#write.csv(parameters,"output_wind_size_and_shapeK.csv",row.names=F)
#write.csv(parameters,"output_wind_size_and_shapeL.csv",row.names=F)
#write.csv(parameters,"output_wind_size_and_shapeM.csv",row.names=F)

