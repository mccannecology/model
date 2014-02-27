#####################################################
# Individual Based Model: Floating plant growth     #
#                                                   #
# By: Michael J. McCann                             #
# Last Updated: 02/26/2014                          #
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
# Make functions flexible diff. # of spp. (I might already be there, just set initial=0 and spp is not included) (fix PLOT2?)
# agedead - as a distribution isntead of a constant
# Print parameter value labels on outputs (.gif or .jpg files)
# Are there any other outputs that I need to save? 
# Simplify calling all functions with one script - almost there 
# Parameters as inputs of a .csv file (whichever is easier for cluster)
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
########################################################################################################
# PARAMETERS  
########################################################################################################
height<-100          # height of the grid
width<-625           # width of the grid

timesteps<-150       # number of time-steps (+1, actually) in a "growing season" 
years<-10            # number of years ("growing seasons") to run the model 

wind_prob<-0.25 # proportion of time steps where wind knocks a row/col off of the grid 
wind_directions<-c("U","L")  # UP, DOWN, LEFT, RIGHT

buffer<-3            # distance from focal cell - used to count up the number of neighbors 

numbspecies <- 4

initial01<-10        # initial number of individuals - species 01
initial02<-10        # initial number of individuals - species 02
initial03<-10        # initial number of individuals - species 03
initial04<-10        # initial number of individuals - species 04
 
agedead01<-35        # average age that individuals die at - species 01
agedead02<-35        # average age that individuals die at - species 02
agedead03<-35        # average age that individuals die at - species 03
agedead04<-35        # average age that individuals die at - species 04

maxrgr01<-0.4        # maximum relative growth rate - species 01
maxrgr02<-0.35       # maximum relative growth rate - species 02
maxrgr03<-0.25       # maximum relative growth rate - species 03
maxrgr04<-0.2        # maximum relative growth rate - species 04

overwinter01<-0.01   # proportion of individuals that overwinter - species 01  
overwinter02<-0.05   # proportion of individuals that overwinter - species 02
overwinter03<-0.075  # proportion of individuals that overwinter - species 03  
overwinter04<-0.1    # proportion of individuals that overwinter - species 04

winters <- (timesteps+1) * seq(from=1, to=years, by=1) # ID timesteps that are winters  
totaltime<- 1+(timesteps+1)*years # total length of time - useful for plotting 

########################################################################################################
# SETUP: Working directory, load functions in the .../FUNCTIONS directory, etc. 
########################################################################################################
# Set the working directory 
# If I am using my laptop
setwd("C:/Users/Mike/Desktop/Dropbox/Duckweed Modelling") 
# If I am using office computer
setwd("C:/Documents and Settings/Lerdau Lab/Desktop/Dropbox/Duckweed Modelling") 
# If I am working off of the flash drive - Note: make sure I get the right root directory (e.g., "E:/")
setwd("E:/Duckweed Modelling") # problem - depending on the computer - this may be E:/ or F:/ 

library(R.utils)
# Be careful! This will load all of the R scripts in your working directory
# BUT this is WAY faster than copying and pasting the function into the console 
sourceDirectory(path=paste(getwd(),"/FUNCTIONS",sep=""),recursive=FALSE) 
########################################################################################################
# Set up the data LIST - starts out time step 0, but LIST[[1]]  
########################################################################################################
simulnumb <- 3

INPUT(simulnumb)

SPECIES()

winters <- (timesteps+1) * seq(from=1, to=years, by=1) # ID timesteps that are winters  

totaltime<- 1+(timesteps+1)*years # total length of time - useful for plotting 

LIST <- vector("list",(1+(timesteps+1)*years)) # Creates the "blank" LIST of lists - PA,AGE - all 0s

for (i in 1:(1+(timesteps+1)*years)){ # fills out the list w/ matrices of 0 
  LIST[[i]] <- BLANK()
}

LIST[[1]]<-START3() # Seed the first time step with some individuals 

#############################
# RUNNING & TIMING THE MODEL
#############################
system.time(LIST<-STEP9()) 
system.time(OUTPUT())
