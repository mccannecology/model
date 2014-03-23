#####################################################
# Accepts .csv of parameter values                  #
# Assigns values to objects in environment          #
# STILL IN DEVELOPMENT                              #
#                                                   #
# By: Michael J. McCann                             #
# Last Updated: 2/26/2014                           #
##################################################### 
INPUT <- function(x){  
  assign("height", parameters$height[simulnumb], envir = .GlobalEnv)  # height of the grid
  assign("width", parameters$width[simulnumb], envir = .GlobalEnv)   # width of the grid
  
  assign("timesteps", parameters$timesteps[simulnumb], envir = .GlobalEnv)  # number of time-steps (+1, actually) in a "growing season" 
  assign("years", parameters$years[simulnumb], envir = .GlobalEnv)      # number of years ("growing seasons") to run the model 
  
  assign("wind_prob", parameters$wind_prob[simulnumb], envir = .GlobalEnv)  # proportion of time steps where wind knocks a row/col off of the grid 
  assign("wind_directions", c(as.character(parameters$wind_directions[simulnumb]),as.character(parameters$wind_directions.1[simulnumb]),as.character(parameters$wind_directions.2[simulnumb]),as.character(parameters$wind_directions.3[simulnumb])), envir = .GlobalEnv)
  
  assign("buffer", parameters$buffer[simulnumb], envir = .GlobalEnv)    # distance from focal cell - used to count up the number of neighbors 
  
  assign("numbspecies", parameters$numbspecies[simulnumb], envir = .GlobalEnv) # number of species in the model 
  
  assign("initial01", parameters$initial01[simulnumb], envir = .GlobalEnv)   # initial number of individuals - species 01
  assign("initial02", parameters$initial02[simulnumb], envir = .GlobalEnv)   # initial number of individuals - species 02
  assign("initial03", parameters$initial03[simulnumb], envir = .GlobalEnv)   # initial number of individuals - species 03
  assign("initial04", parameters$initial04[simulnumb], envir = .GlobalEnv)   # initial number of individuals - species 04
  
  assign("agedead01", parameters$agedead01[simulnumb], envir = .GlobalEnv)   # average age that individuals die at - species 01
  assign("agedead02", parameters$agedead02[simulnumb], envir = .GlobalEnv)   # average age that individuals die at - species 02
  assign("agedead03", parameters$agedead03[simulnumb], envir = .GlobalEnv)   # average age that individuals die at - species 03
  assign("agedead04", parameters$agedead04[simulnumb], envir = .GlobalEnv)   # average age that individuals die at - species 04
  
  assign("maxrgr01", parameters$maxrgr01[simulnumb], envir = .GlobalEnv)    # maximum relative growth rate - species 01
  assign("maxrgr02", parameters$maxrgr02[simulnumb], envir = .GlobalEnv)    # maximum relative growth rate - species 02
  assign("maxrgr03", parameters$maxrgr03[simulnumb], envir = .GlobalEnv)    # maximum relative growth rate - species 03
  assign("maxrgr04", parameters$maxrgr04[simulnumb], envir = .GlobalEnv)    # maximum relative growth rate - species 04
  
  assign("overwinter01", parameters$overwinter01[simulnumb], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 01  
  assign("overwinter02", parameters$overwinter02[simulnumb], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 02
  assign("overwinter03", parameters$overwinter03[simulnumb], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 03  
  assign("overwinter04", parameters$overwinter04[simulnumb], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 04
}