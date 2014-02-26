#####################################################
# Creates data frame of parameter values to input   #
# STILL IN DEVELOPMENT                              #
#                                                   #
# By: Michael J. McCann                             #
# Last Updated: 2/26/2014                           #
##################################################### 

input <- read.csv("input.csv")

simulation <- 1 

height <- input[simulation,1]          # height of the grid
width  <- input[simulation,2]           # width of the grid

timesteps <- input[simulation,3]       # number of time-steps (+1, actually) in a "growing season" 
years <- input[simulation,4]           # number of years ("growing seasons") to run the model 

wind_prob <- input[simulation,5]  # proportion of time steps where wind knocks a row/col off of the grid 

wind_directions <- c(as.character(input[simulation,6]),as.character(input[simulation,7]),as.character(input[simulation,8]),as.character(input[simulation,9]))

buffer <- input[simulation,10]            # distance from focal cell - used to count up the number of neighbors 

numbspecies <- input[simulation,11]

initial01 <- input[simulation,12]        # initial number of individuals - species 01
initial02 <- input[simulation,13]        # initial number of individuals - species 02
initial03 <- input[simulation,14]        # initial number of individuals - species 03
initial04 <- input[simulation,15]        # initial number of individuals - species 04

agedead01 <- input[simulation,16]        # average age that individuals die at - species 01
agedead02 <- input[simulation,17]        # average age that individuals die at - species 02
agedead03 <- input[simulation,18]        # average age that individuals die at - species 03
agedead04 <- input[simulation,19]        # average age that individuals die at - species 04

maxrgr01 <- input[simulation,20]      # maximum relative growth rate - species 01
maxrgr02 <- input[simulation,21]      # maximum relative growth rate - species 02
maxrgr03 <- input[simulation,22]      # maximum relative growth rate - species 03
maxrgr04 <- input[simulation,23]      # maximum relative growth rate - species 04

overwinter01 <- input[simulation,24]   # proportion of individuals that overwinter - species 01  
overwinter02 <- input[simulation,25]   # proportion of individuals that overwinter - species 02
overwinter03 <- input[simulation,26]   # proportion of individuals that overwinter - species 03  
overwinter04 <- input[simulation,27]   # proportion of individuals that overwinter - species 04