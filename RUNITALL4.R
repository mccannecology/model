################################################
# Spatially-explicit Floating plant growth     #
# Now compatible with new LIST structure       #
#                                              #
# By: Michael J. McCann                        #
# Major revision: 03/24/2014                   #
################################################
# 
# CURRENT SETUP:
# Multiple species of duckweed growing - Each species is on their own 2-d grid (matrix)
# Positions of plants based on X,Y coordinates in a matrix   
# Includes an overwintering step, where there is a large die-off of individuals 
#
# TO DO:
# add WIND() 
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
# Enter your total number of simulations - should be same as # rows in "inputXX.csv"
totalsimuls <- 3

# imports parameter  values for all simulations 
parameters <- read.csv("input05.csv") 

# add blank columns to parameters for each of the results 
parameters$propyears_avgFPcover_abovethreshold <- rep(NA, totalsimuls)
parameters$propyears_prop_daysFP_abovehalf <- rep(NA, totalsimuls)
parameters$avg_avg_FPcover <- rep(NA, totalsimuls)
parameters$avg_max_FPcover <- rep(NA, totalsimuls)
parameters$avg_firstdayFP <- rep(NA, totalsimuls)
parameters$propyears_avgSAVcover_abovethreshold <- rep(NA, totalsimuls)
parameters$propyears_prop_daysSAV_abovehalf <- rep(NA, totalsimuls)
parameters$avg_avg_SAVcover <- rep(NA, totalsimuls)
parameters$avg_max_SAVcover <- rep(NA, totalsimuls)
parameters$avg_firstdaySAV <- rep(NA, totalsimuls)

# load the packages you need 
require(foreach)
require(doSNOW)
require(R.utils) # package for sourceDirectory() - loeding all the functions in a directory 

# make the correct number of clusters - the first argument will change depending on the machine / set-up 
cl <- makeCluster(3,"SOCK") 

# load all your functions
sourceDirectory(path=paste(getwd(),"/FUNCTIONS",sep=""),recursive=FALSE) 

#  assigns the functions to the global environments of each node
clusterExport(cl, c("BLANK3", "GROW_SAV", "GROW_FP", "INPUT4","MOVE_FP","MOVE_SAV",
                    "OUTPUT4","OVERWINTER","RELEASE_N2","RELEASE_P2",
                    "SPECIES4","START5","STEP11","UPTAKE_N2","UPTAKE_P2"))

registerDoSNOW(cl) # registers the SNOW parallel backend w/ foreach package 
getDoParWorkers() # returns the # of workers - this should match the # of cores on your machine (or # cores - 1)

RESULT <- foreach (i=1:nrow(parameters), .combine=rbind) %dopar% { # loop through all of your simulations - User needs to specify the max # of simulations (rows of parameters) in .csv
  # I am not sure if this is neccasary. Can I just use i as the input to the functions below?
  simulnumb <- i # assigns the simulation # from the for loop - will be used as an input to INPUT() to read the right row of .csv
  
  INPUT4(simulnumb) # reads the .csv file of parameter values and assigns them to the global environment 
  
  speciesmatrix <- SPECIES4(simulnumb) # function that builds the dataframe of species-specific parameters that is used in STEPX()
  
  # define couple of things in the environment that get used in STEPX() and OUTPUT()
  winters <- (timesteps+1) * seq(from=1, to=years, by=1) # ID timesteps that are winters - used in STEPX()
  
  totaltime<- 1+(timesteps+1)*years # total length of time - used in OUTPUT() plotting
  
  LIST <- vector("list",(1+(timesteps+1)*years)) # Creates the "blank" LIST 
  
  for (i in 1:(1+(timesteps+1)*years)){ # fills  the LIST w/ matrices of 0 
    LIST[[i]] <- BLANK3()
  }
  
  LIST[[1]]<-START5() # Start the first time step with some individuals 
  
  LIST<-STEP11() # Run the model for all of the time steps - aging, senescence, reproduction, overwintering, movement, etc. 
  
  OUTPUT4(threshold=70) # generates graphs - if you want .html animation you must specify animate=TRUE, set "FP regime" threshold here too 
  
  # RESULTS[simulnumb,1] <- propyears_avgFPcover_abovethreshold # assign the current simulations results to the correct spot
  # RESULTS[simulnumb,2] <- propyears_prop_daysFP_abovehalf # assign the current simulations results to the correct spot
  # RESULTS[simulnumb,3] <- avg_avg_FPcover # assign the current simulations results to the correct spot  
  
  # parameters$propyears_avgFPcover_abovethreshold[simulnumb] <- propyears_avgFPcover_abovethreshold
  # parameters$propyears_prop_daysFP_abovehalf[simulnumb] <- propyears_prop_daysFP_abovehalf
  # parameters$avg_avg_FPcover[simulnumb] <- avg_avg_FPcover
    
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
write.csv(parameters,"output05.csv",row.names=F) 

