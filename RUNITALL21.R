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
# Modify shape of the waterbody (non-rectangular)
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
# Does the order of evaluting individuals in MOVE() matter? (i.e., center vs. UL corner vs. UR corner, etc.) - probably not b/c don't require empty cell 
# 
########################################################################################################

########################################
# load workspace for de-bugging
# 
# LIST has an initial time step only 
# short (3 yrs, 50 days each)
# one FP species
# load("testworkspace.Rdata") 
# two FP species
# load("testworkspace-2FPspecies.Rdata") 
# four FP species
# load("testworkspace-4FPspecies.Rdata") 
#
# complete LIST 
# 7 years, 125 days each 
# load("testworkspace-complete.Rdata")
########################################

# imports parameter  values for all simulations 
parameters <- read.csv("input21.csv")

# add blank columns to parameters for each of the results 
parameters$propyears_avgFPcover_abovethreshold <- rep(NA, nrow(parameters))
parameters$propyears_prop_daysFP_abovehalf <- rep(NA, nrow(parameters))
parameters$avg_avg_FPcover <- rep(NA, nrow(parameters))
parameters$avg_max_FPcover <- rep(NA, nrow(parameters))
parameters$avg_firstdayFP <- rep(NA, nrow(parameters))
parameters$propyears_avgSAVcover_abovethreshold <- rep(NA, nrow(parameters))
parameters$propyears_prop_daysSAV_abovehalf <- rep(NA, nrow(parameters))
parameters$avg_avg_SAVcover <- rep(NA, nrow(parameters))
parameters$avg_max_SAVcover <- rep(NA, nrow(parameters))
parameters$avg_firstdaySAV <- rep(NA, nrow(parameters))

# load the packages you need 
require(foreach)
require(doSNOW)
require(R.utils) # package for sourceDirectory() - loeding all the functions in a directory 

# make the correct number of clusters - the first argument will change depending on the machine / set-up 
cl <- makeCluster(4,"SOCK") 

# load all your functions
sourceDirectory(path=paste(getwd(),"/FUNCTIONS",sep=""),recursive=FALSE) 

#  assigns the functions to the global environments of each node
clusterExport(cl, c("BLANK21", "GROW_SAV21", "GROW_FP21", 
                    "INPUT21","MOVE21",
                    "OUTPUT21","SPECIES21","START21",
                    "STEP21","UPTAKE_N21","UPTAKE_P21","WIND21"))

registerDoSNOW(cl) # registers the SNOW parallel backend w/ foreach package 
getDoParWorkers() # returns the # of workers - this should match the # of cores on your machine (or # cores - 1)

RESULT <- foreach (i=1:nrow(parameters), .combine=rbind) %dopar% { # loop through all of your simulations - User needs to specify the max # of simulations (rows of parameters) in .csv
  # I am not sure if this is neccasary. Can I just use i as the input to the functions below?
  simulnumb <- i # assigns the simulation # from the for loop - will be used as an input to INPUT() to read the right row of .csv
  
  INPUT21(simulnumb) # reads the .csv file of parameter values and assigns them to the global environment 
  
  # speciesmatrix <- SPECIES20(simulnumb) # function that builds the dataframe of species-specific parameters that is used in STEPX()
  specieslist <- SPECIES21() # function that builds the list of species-specific parameters that is used in STEPX()  
  
  # define a couple of things in the environment that get used in STEPX() and OUTPUT()
  timesteps <- years*(days+1) # this will need to change b/c of overwintering 
  winters <- (days+1) * seq(from=1, to=years, by=1) # ID timesteps that are winters - used in STEPX()
  
  # generate blank list for total timesteps
  LIST<-NULL
  for (i in 1:timesteps){ 
    LIST[[i]] <- BLANK21()
  }
  
  # initialize first time step
  LIST[[1]]<-START21(LIST[[1]])  
  
  # for loop - STEP20() to the entire LIST
  today<-LIST[[1]]
  for (t in 1:timesteps){
    tomorrow<-STEP21(today,t)
    LIST[[t+1]]<-tomorrow
    today<-tomorrow
    
    ##################################
    # Plot as you go (slows it down) #
    ##################################
    require(raster)
    
    # make raster layers 
    SAV<-raster(LIST[[t]]$SAV)
    for (y in 1:numbFPspecies){
      assign(paste("FP0",y,sep=""),raster(LIST[[t]]$FP[[y]]))
    }
    FPtotal<-raster(LIST[[t]]$FPtotal)
     
    # stack raster layers 
    # I need a smarter way to make this variable length 
    if (numbFPspecies == 4){
      all_layers <- stack(SAV,FPtotal,FP01,FP02,FP03,FP04)
    }
    if (numbFPspecies == 3){
      all_layers <- stack(SAV,FPtotal,FP01,FP02,FP03)
    }
    if (numbFPspecies == 2){
      all_layers <- stack(SAV,FPtotal,FP01,FP02)
    }
    if (numbFPspecies == 1){
      all_layers <- stack(SAV,FPtotal,FP01)
    }
    
    # name raster layers 
    names(all_layers)[1] <- "SAV"
    names(all_layers)[2] <- "FPtotal"
    for (y in 1:numbFPspecies){
      names(all_layers)[y+2] <- paste("FP0",y,sep="")
    }
    
    # plot raster layers 
    plot(all_layers)
    
    # print timestep to console - SLOW!
    # print(t)
  }    
  
  # generates graphs
  # if you want .html animation you must specify animate=TRUE
  # set "FP regime" threshold here  
  OUTPUT21(animate=FALSE,regimethreshold=70) 
  
  # RESULTS[simulnumb,1] <- propyears_avgFPcover_abovethreshold # assign the current simulations results to the correct spot
  # RESULTS[simulnumb,2] <- propyears_prop_daysFP_abovehalf # assign the current simulations results to the correct spot
  # RESULTS[simulnumb,3] <- avg_avg_FPcover # assign the current simulations results to the correct spot  
  
  # parameters$propyears_avgFPcover_abovethreshold[simulnumb] <- propyears_avgFPcover_abovethreshold
  # parameters$propyears_prop_daysFP_abovehalf[simulnumb] <- propyears_prop_daysFP_abovehalf
  # parameters$avg_avg_FPcover[simulnumb] <- avg_avg_FPcover
    
  # these results are produced by OUTPUT()
  # stick all of the results you want out in a vector together 
  c(simulnumb, 
    propyears_avgFPcover_abovethreshold,
    propyears_prop_daysFP_abovehalf,
    avg_avg_FPcover,
    avg_max_FPcover,
    avg_firstdayFP,
    propyears_avgSAVcover_abovethreshold,
    propyears_prop_daysSAV_abovehalf,
    avg_avg_SAVcover,
    avg_max_SAVcover,
    avg_firstdaySAV)
    
}

# stop the cluster 
stopCluster(cl)

# name the RESULTS columns 
colnames(RESULT) <- c("simulnumb",
                      "propyears_avgFPcover_abovethreshold",
                      "propyears_prop_daysFP_abovehalf",
                      "avg_avg_FPcover", 
                      "avg_max_FPcover", 
                      "avg_firstdayFP",
                      "propyears_avgSAVcover_abovethreshold",
                      "propyears_prop_daysSAV_abovehalf",
                      "avg_avg_SAVcover", 
                      "avg_max_SAVcover", 
                      "avg_firstdaySAV")

# convert to a data frame 
RESULT <- as.data.frame(RESULT)

# Order the RESULT by simulation number 
# create a vector of simulnumb in increasing order 
order.simulnumb <- order(RESULT$simulnumb)

# use that vector to order the RESULT data frame 
RESULT <- RESULT[order.simulnumb,] 

# add the columns of the RESULT data frame to the original parameters data frame 
parameters$propyears_avgFPcover_abovethreshold <- RESULT[,2]
parameters$propyears_prop_daysFP_abovehalf <- RESULT[,3]
parameters$avg_avg_FPcover <- RESULT[,4]
parameters$avg_max_FPcover <- RESULT[,5]
parameters$avg_firstdayFP <- RESULT[,6]
parameters$propyears_avgSAVcover_abovethreshold <- RESULT[,7]
parameters$propyears_prop_daysSAV_abovehalf <- RESULT[,8]
parameters$avg_avg_SAVcover <- RESULT[,9]
parameters$avg_max_SAVcover <- RESULT[,10]
parameters$avg_firstdaySAV <- RESULT[,11]


# write parameters with RESULT appended to a .csv 
write.csv(parameters,"output20.csv",row.names=F) 

