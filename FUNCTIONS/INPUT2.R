#####################################################
# Accepts .csv of parameter values                  #
# Assigns values to objects in environment          #
# Compatible with new LIST structure                #
#                                                   #
# By: Michael J. McCann                             #
# Major revision: 3/24/2014                         #
##################################################### 
INPUT2 <- function(x){  
  assign("height", parameters$height[simulnumb], envir = .GlobalEnv)  # height of the grid
  assign("width", parameters$width[simulnumb], envir = .GlobalEnv)   # width of the grid
  
  assign("timesteps", parameters$timesteps[simulnumb], envir = .GlobalEnv)  # number of time-steps (+1, actually) in a "growing season" 
  assign("years", parameters$years[simulnumb], envir = .GlobalEnv)      # number of years ("growing seasons") to run the model 
  
  assign("TOTALN", parameters$TOTALN[simulnumb], envir = .GlobalEnv)  # overall nitrogen concentration (mg/L)
  assign("TOTALP", parameters$TOTALP[simulnumb], envir = .GlobalEnv)  # overall phosphorus concentration (mg/L)
  
  assign("loss", parameters$loss[simulnumb], envir = .GlobalEnv)  # "losses" - respiration & various sources of mortality 
    
  assign("numbspecies", parameters$numbspecies[simulnumb], envir = .GlobalEnv) # number of species in the model 
  
  assign("initial01cells", parameters$initial01cells[simulnumb], envir = .GlobalEnv)   # initial number of cells occupied - species 01
  assign("initial02cells", parameters$initial02cells[simulnumb], envir = .GlobalEnv)   # initial number of cells occupied - species 02
  assign("initial03cells", parameters$initial03cells[simulnumb], envir = .GlobalEnv)   # initial number of cells occupied - species 03
  assign("initial04cells", parameters$initial04cells[simulnumb], envir = .GlobalEnv)   # initial number of cells occupied - species 04

  assign("initial01totmass", parameters$initial01totmass[simulnumb], envir = .GlobalEnv)   # initial total biomass - species 01
  assign("initial02totmass", parameters$initial02totmass[simulnumb], envir = .GlobalEnv)   # initial total biomass - species 02
  assign("initial03totmass", parameters$initial03totmass[simulnumb], envir = .GlobalEnv)   # initial total biomass - species 03
  assign("initial04totmass", parameters$initial04totmass[simulnumb], envir = .GlobalEnv)   # initial total biomass - species 04
  
  assign("maxrgr01", parameters$maxrgr01[simulnumb], envir = .GlobalEnv)    # maximum relative growth rate - species 01
  assign("maxrgr02", parameters$maxrgr02[simulnumb], envir = .GlobalEnv)    # maximum relative growth rate - species 02
  assign("maxrgr03", parameters$maxrgr03[simulnumb], envir = .GlobalEnv)    # maximum relative growth rate - species 03
  assign("maxrgr04", parameters$maxrgr04[simulnumb], envir = .GlobalEnv)    # maximum relative growth rate - species 04
  
  assign("overwinter01", parameters$overwinter01[simulnumb], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 01  
  assign("overwinter02", parameters$overwinter02[simulnumb], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 02
  assign("overwinter03", parameters$overwinter03[simulnumb], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 03  
  assign("overwinter04", parameters$overwinter04[simulnumb], envir = .GlobalEnv)    # proportion of individuals that overwinter - species 04
  
  assign("halfsatB01", parameters$halfsatB01[simulnumb], envir = .GlobalEnv)    # biomass half saturation - species 01  
  assign("halfsatB02", parameters$halfsatB02[simulnumb], envir = .GlobalEnv)    # biomass half saturation - species 02
  assign("halfsatB03", parameters$halfsatB03[simulnumb], envir = .GlobalEnv)    # biomass half saturation - species 03  
  assign("halfsatB04", parameters$halfsatB04[simulnumb], envir = .GlobalEnv)    # biomass half saturation - species 04
  
  assign("halfsatN01", parameters$halfsatN01[simulnumb], envir = .GlobalEnv)    # nitrogen half saturation - species 01  
  assign("halfsatN02", parameters$halfsatN02[simulnumb], envir = .GlobalEnv)    # nitrogen half saturation - species 02
  assign("halfsatN03", parameters$halfsatN03[simulnumb], envir = .GlobalEnv)    # nitrogen half saturation - species 03  
  assign("halfsatN04", parameters$halfsatN04[simulnumb], envir = .GlobalEnv)    # nitrogen half saturation - species 04
  
  assign("halfsatP01", parameters$halfsatP01[simulnumb], envir = .GlobalEnv)    # phosphorus half saturation - species 01  
  assign("halfsatP02", parameters$halfsatP02[simulnumb], envir = .GlobalEnv)    # phosphorus half saturation - species 02
  assign("halfsatP03", parameters$halfsatP03[simulnumb], envir = .GlobalEnv)    # phosphorus half saturation - species 03  
  assign("halfsatP04", parameters$halfsatP04[simulnumb], envir = .GlobalEnv)    # phosphorus half saturation - species 04
  
  assign("uptakeN01", parameters$uptakeN01[simulnumb], envir = .GlobalEnv)    # nitrogen uptake rate - species 01  
  assign("uptakeN02", parameters$uptakeN02[simulnumb], envir = .GlobalEnv)    # nitrogen uptake rate - species 02
  assign("uptakeN03", parameters$uptakeN03[simulnumb], envir = .GlobalEnv)    # nitrogen uptake rate - species 03  
  assign("uptakeN04", parameters$uptakeN04[simulnumb], envir = .GlobalEnv)    # nitrogen uptake rate - species 04
  
  assign("uptakeP01", parameters$uptakeP01[simulnumb], envir = .GlobalEnv)    # phosphorus uptake rate - species 01  
  assign("uptakeP02", parameters$uptakeP02[simulnumb], envir = .GlobalEnv)    # phosphorus uptake rate - species 02
  assign("uptakeP03", parameters$uptakeP03[simulnumb], envir = .GlobalEnv)    # phosphorus uptake rate - species 03  
  assign("uptakeP04", parameters$uptakeP04[simulnumb], envir = .GlobalEnv)    # phosphorus uptake rate - species 04
}