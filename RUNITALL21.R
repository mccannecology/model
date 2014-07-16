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
# load("testworkspace-1FPspecies.Rdata") 
# four FP species
# load("testworkspace-4FPspecies.Rdata") 
#
# complete LIST 
# 7 years, 125 days each 
# load("testworkspace-complete.Rdata")
########################################

# imports parameter  values for all simulations 
parameters <- read.csv("input23.csv")

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
                    "INPUT21","MOVE21", "OUTPUT21",
                    "OVERWINTER21","SPECIES21","START21",
                    "STEP21","UPTAKE_N21","UPTAKE_P21","WIND22"))

registerDoSNOW(cl) # registers the SNOW parallel backend w/ foreach package 
getDoParWorkers() # returns the # of workers - this should match the # of cores on your machine (or # cores - 1)

 