#####################################################
# Accepts .csv of parameter values                  #
# Assigns values to objects in environment          #
# STILL IN DEVELOPMENT                              #
#                                                   #
# By: Michael J. McCann                             #
# Last Updated: 2/26/2014                           #
##################################################### 
INPUT <- function(x){
  parameters <- read.csv("input.csv")
  
  simulnumb <- x 
  
  assign("height", parameters[simulnumb,1], envir = .GlobalEnv)  # height of the grid
  width  <- parameters[simulnumb,2]           # width of the grid
  
  timesteps <- parameters[simulnumb,3]       # number of time-steps (+1, actually) in a "growing season" 
  years <- parameters[simulnumb,4]           # number of years ("growing seasons") to run the model 
  
  wind_prob <- parameters[simulnumb,5]  # proportion of time steps where wind knocks a row/col off of the grid 
  
  wind_directions <- c(as.character(parameters[simulnumb,6]),as.character(parameters[simulnumb,7]),as.character(parameters[simulnumb,8]),as.character(parameters[simulnumb,9]))
  assign("wind_directions", c(as.character(parameters[simulnumb,6]),as.character(parameters[simulnumb,7]),as.character(parameters[simulnumb,8]),as.character(parameters[simulnumb,9])), envir = .GlobalEnv)
  
  buffer <- parameters[simulnumb,10]            # distance from focal cell - used to count up the number of neighbors 
  
  numbspecies <- parameters[simulnumb,11]
  
  initial01 <- parameters[simulnumb,12]        # initial number of individuals - species 01
  initial02 <- parameters[simulnumb,13]        # initial number of individuals - species 02
  initial03 <- parameters[simulnumb,14]        # initial number of individuals - species 03
  initial04 <- parameters[simulnumb,15]        # initial number of individuals - species 04
  
  agedead01 <- parameters[simulnumb,16]        # average age that individuals die at - species 01
  agedead02 <- parameters[simulnumb,17]        # average age that individuals die at - species 02
  agedead03 <- parameters[simulnumb,18]        # average age that individuals die at - species 03
  agedead04 <- parameters[simulnumb,19]        # average age that individuals die at - species 04
  
  maxrgr01 <- parameters[simulnumb,20]      # maximum relative growth rate - species 01
  maxrgr02 <- parameters[simulnumb,21]      # maximum relative growth rate - species 02
  maxrgr03 <- parameters[simulnumb,22]      # maximum relative growth rate - species 03
  maxrgr04 <- parameters[simulnumb,23]      # maximum relative growth rate - species 04
  
  overwinter01 <- parameters[simulnumb,24]   # proportion of individuals that overwinter - species 01  
  overwinter02 <- parameters[simulnumb,25]   # proportion of individuals that overwinter - species 02
  overwinter03 <- parameters[simulnumb,26]   # proportion of individuals that overwinter - species 03  
  overwinter04 <- parameters[simulnumb,27]   # proportion of individuals that overwinter - species 04
}