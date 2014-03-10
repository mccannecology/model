#####################################################
# Individual Based Model: Floating plant growth     #
#                                                   #
#                                                   #
# By: Michael J. McCann                             #
# Last Updated: 03/10/2014                          #
#####################################################
# 
# CURRENTLY:
# Multiple species reproducing, aging, dying on 2-d grid (matrix)
# Positions of plants based on X,Y coordinates in a matrix   
# Individuals (1s, 2s, etc) look for nearest empty cell (0) to reproduce into 
# Includes an overwintering step, where there is a large die-off of individuals 
#
# TO DO:
# Modify MOVEMENT: don't spill off edge - pile up instead 
# Modify shape of the waterbody (non-rectangular)
#
# agedead - as a distribution isntead of a constant
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
# Does the order of evaluting individuals in STEP matter? (i.e., center vs. UL corner vs. UR corner, etc.)
# Is it ok to use LIST[[i]]$RAND for choosing overwintering deaths in WINTER.R? (also used for maxrgr - on previous steps) 
#
# 
########################################################################################################
# Enter your total number of simulations 
totalsimuls <- 180 

# set-up blank vectors for any of the results 
VECTOR_propyears_avgFP_abovethreshold <- rep(NA, totalsimuls)
VECTOR_propyears_propdaysFP_abovehalf <- rep(NA, totalsimuls)
RESULTS <- data.frame(VECTOR_propyears_avgFP_abovethreshold,VECTOR_propyears_propdaysFP_abovehalf)

parameters <- read.csv("input04.csv") # imports parameter  values for all simulations 

for (i in 1:180) { # loop through all of your simulations - User needs to specify the max # of simulations (rows of parameters) in .csv
  require(R.utils) # package for sourceDirectory()
  
  sourceDirectory(path=paste(getwd(),"/FUNCTIONS",sep=""),recursive=FALSE) # load all your functions
  
  simulnumb <- i # assigns the simulation # from the for loop - will be used as an input to INPUT() to read the right row of .csv
  
  INPUT(simulnumb) # reads the .csv file of parameter values and assigns them to the global environment 
  
  SPECIES(simulnumb) # function that builds the dataframe of species-specific parameters that is used in STEPX()
  
  # define couple of things in the global environment that get used in STEPX() and OUTPUT()
  
  winters <- (timesteps+1) * seq(from=1, to=years, by=1) # ID timesteps that are winters - used in STEPX()
  
  totaltime<- 1+(timesteps+1)*years # total length of time - used in OUTPUT() plotting
  
  LIST <- vector("list",(1+(timesteps+1)*years)) # Creates the "blank" LIST 
  
  for (i in 1:(1+(timesteps+1)*years)){ # fills  the LIST w/ matrices of 0 
    LIST[[i]] <- BLANK()
  }
  
  LIST[[1]]<-START3() # Start the first time step with some individuals 

  LIST<-STEP9() # Runs the model for all of the time steps - aging, senescence, reproduction, overwintering, movement, etc. 
  
  OUTPUT() # generates graphs - if you want .html animation you must specify ani
  
  RESULTS[simulnumb,1] <- propyears_avgFP_abovethreshold # assign the current simulations results to the correct spot
  RESULTS[simulnumb,2] <- propyears_propdaysFP_abovehalf # assign the current simulations results to the correct spot
    
  rm(list = ls()[!(ls() %in% c("RESULTS","parameters"))]) # clear workspace (except for RESULTS and parameters) for next simulation 
  
}

# add the results vectors to the original parameters data frame 
parameters$propyears_avgFP_abovethreshold <- RESULTS[,1]
parameters$propyears_propdaysFP_abovehalf <- RESULTS[,2]

# add these results to your original input file 
write.csv(parameters,"input04.csv",append=T,row.names=F) 

