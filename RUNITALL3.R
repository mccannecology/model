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

# load the packages you need 
require(foreach)
require(doSNOW)
require(R.utils) # package for sourceDirectory() - loeding all the functions in a directory 

# make the correct number of clusters - the first argument will change depending on the machine / set-up 
cl <- makeCluster(2,"SOCK") 

# load all your functions
sourceDirectory(path=paste(getwd(),"/FUNCTIONS",sep=""),recursive=FALSE) 

#  assigns the functions to the global environments of each node
clusterExport(cl, c("BLANK2", "GROW", "INPUT2","MOVE",
                    "OUTPUT2","OVERWINTER","RELEASE_N","RELEASE_P",
                    "SPECIES2","START4","STEP10","UPTAKE_N","UPTAKE_P"))

registerDoSNOW(cl) # registers the SNOW parallel backend w/ foreach package 
getDoParWorkers() # returns the # of workers - this should match the # of cores on your machine (or # cores - 1)

RESULT <- foreach (i=1:nrow(parameters), .combine=rbind) %dopar% { # loop through all of your simulations - User needs to specify the max # of simulations (rows of parameters) in .csv
  # I am not sure if this is neccasary. Can I just use i as the input to the functions below?
  simulnumb <- i # assigns the simulation # from the for loop - will be used as an input to INPUT() to read the right row of .csv
  
  INPUT3(simulnumb) # reads the .csv file of parameter values and assigns them to the global environment 
  
  speciesmatrix <- SPECIES3(simulnumb) # function that builds the dataframe of species-specific parameters that is used in STEPX()
  
  # define couple of things in the environment that get used in STEPX() and OUTPUT()
  
  winters <- (timesteps+1) * seq(from=1, to=years, by=1) # ID timesteps that are winters - used in STEPX()
  
  totaltime<- 1+(timesteps+1)*years # total length of time - used in OUTPUT() plotting
  
  LIST <- vector("list",(1+(timesteps+1)*years)) # Creates the "blank" LIST 
  
  for (i in 1:(1+(timesteps+1)*years)){ # fills  the LIST w/ matrices of 0 
    LIST[[i]] <- BLANK2()
  }
  
  LIST[[1]]<-START4() # Start the first time step with some individuals 
  
  LIST<-STEP10() # Runs the model for all of the time steps - aging, senescence, reproduction, overwintering, movement, etc. 
  
  OUTPUT3(threshold=70) # generates graphs - if you want .html animation you must specify animate=TRUE, set "FP regime" threshold here too 
  
  # RESULTS[simulnumb,1] <- propyears_avgFPcover_abovethreshold # assign the current simulations results to the correct spot
  # RESULTS[simulnumb,2] <- propyears_prop_daysFP_abovehalf # assign the current simulations results to the correct spot
  # RESULTS[simulnumb,3] <- avg_avg_FPcover # assign the current simulations results to the correct spot  
  
  # parameters$propyears_avgFPcover_abovethreshold[simulnumb] <- propyears_avgFPcover_abovethreshold
  # parameters$propyears_prop_daysFP_abovehalf[simulnumb] <- propyears_prop_daysFP_abovehalf
  # parameters$avg_avg_FPcover[simulnumb] <- avg_avg_FPcover
    
  c(simulnumb, propyears_avgFPcover_abovethreshold,propyears_prop_daysFP_abovehalf,avg_avg_FPcover)
  
  # rm(list = ls()[!(ls() %in% c("RESULTS","parameters"))]) # clear workspace (except for RESULTS and parameters) for next simulation 
  
}

# stop the cluster 
stopCluster(cl)

# name the RESULTS columns 
colnames(RESULT) <- c("simulnumb", "propyears_avgFPcover_abovethreshold","propyears_prop_daysFP_abovehalf","avg_avg_FPcover")

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

# write parameters with RESULT appended to a .csv 
write.csv(parameters,"output05.csv",row.names=F) 

