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
  
  assign("parameters", parameters, envir = .GlobalEnv)
  
  simulnumb <- x 
  
  assign("height", parameters[simulnumb,1], envir = .GlobalEnv)  # height of the grid
  assign("width", parameters[simulnumb,2], envir = .GlobalEnv)   # width of the grid
  
  assign("timesteps", parameters[simulnumb,3], envir = .GlobalEnv)  # number of time-steps (+1, actually) in a "growing season" 
  assign("years", parameters[simulnumb,4], envir = .GlobalEnv)      # number of years ("growing seasons") to run the model 
  
  assign("wind_prob", parameters[simulnumb,5], envir = .GlobalEnv)  # proportion of time steps where wind knocks a row/col off of the grid 
  assign("wind_directions", c(as.character(parameters[simulnumb,6]),as.character(parameters[simulnumb,7]),as.character(parameters[simulnumb,8]),as.character(parameters[simulnumb,9])), envir = .GlobalEnv)
  
  assign("buffer", parameters[simulnumb,10], envir = .GlobalEnv)    # distance from focal cell - used to count up the number of neighbors 
  
  assign("numbspecies", parameters[simulnumb,11], envir = .GlobalEnv) # number of species in the model 
  
  assign("initial01", parameters[simulnumb,12], envir = .GlobalEnv)   # initial number of individuals - species 01
  assign("initial02", parameters[simulnumb,13], envir = .GlobalEnv)   # initial number of individuals - species 02
  assign("initial03", parameters[simulnumb,14], envir = .GlobalEnv)   # initial number of individuals - species 03
  assign("initial04", parameters[simulnumb,15], envir = .GlobalEnv)   # initial number of individuals - species 04
  
  assign("agedead01", parameters[simulnumb,16], envir = .GlobalEnv)   # average age that individuals die at - species 01
  assign("agedead02", parameters[simulnumb,17], envir = .GlobalEnv)   # average age that individuals die at - species 02
  assign("agedead03", parameters[simulnumb,18], envir = .GlobalEnv)   # average age that individuals die at - species 03
  assign("agedead04", parameters[simulnumb,19], envir = .GlobalEnv)   # average age that individuals die at - species 04
  
  assign("maxrgr01", parameters[simulnumb,20], envir = .GlobalEnv)    # maximum relative growth rate - species 01
  assign("maxrgr02", parameters[simulnumb,21], envir = .GlobalEnv)    # maximum relative growth rate - species 02
  assign("maxrgr03", parameters[simulnumb,22], envir = .GlobalEnv)    # maximum relative growth rate - species 03
  assign("maxrgr04", parameters[simulnumb,23], envir = .GlobalEnv)    # maximum relative growth rate - species 04
  
  assign("overwinter01", parameters[simulnumb,24], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 01  
  assign("overwinter02", parameters[simulnumb,25], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 02
  assign("overwinter03", parameters[simulnumb,26], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 03  
  assign("overwinter04", parameters[simulnumb,27], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 04
}