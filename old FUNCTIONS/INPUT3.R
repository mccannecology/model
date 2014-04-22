#####################################################
# Accepts .csv of parameter values                  #
# Assigns values to objects in environment          #
# Compatible with new LIST structure                #
#                                                   #
# By: Michael J. McCann                             #
# Major revision: 3/24/2014                         #
##################################################### 
INPUT3 <- function(x){  
  assign("height", parameters$height[simulnumb], pos = 1)  # height of the grid
  assign("width", parameters$width[simulnumb], pos = 1)   # width of the grid
  
  assign("timesteps", parameters$timesteps[simulnumb], pos = 1)  # number of time-steps (+1, actually) in a "growing season" 
  assign("years", parameters$years[simulnumb], pos = 1)      # number of years ("growing seasons") to run the model 
  
  assign("TOTALN", parameters$TOTALN[simulnumb], pos = 1)  # overall nitrogen concentration (mg/L)
  assign("TOTALP", parameters$TOTALP[simulnumb], pos = 1)  # overall phosphorus concentration (mg/L)
  
  assign("loss", parameters$loss[simulnumb], pos = 1)  # "losses" - respiration & various sources of mortality 
  
  assign("thresholdtomove", parameters$thresholdtomove[simulnumb], pos = 1)  # threshold value - above this biomass and plants start moving 
  assign("maxthresholdtomove", parameters$maxthresholdtomove[simulnumb], pos = 1)  # max threshold value - above this biomass and move at a constant maxamountomove value
  assign("maxamounttomove", parameters$maxamounttomove[simulnumb], pos = 1)  # movement amount once plant density is at maxthresholdtomove 
  
  assign("numbspecies", parameters$numbspecies[simulnumb], pos = 1) # number of species in the model 
  
  assign("initial01cells", parameters$initial01cells[simulnumb], pos = 1)   # initial number of cells occupied - species 01
  assign("initial02cells", parameters$initial02cells[simulnumb], pos = 1)   # initial number of cells occupied - species 02
  assign("initial03cells", parameters$initial03cells[simulnumb], pos = 1)   # initial number of cells occupied - species 03
  assign("initial04cells", parameters$initial04cells[simulnumb], pos = 1)   # initial number of cells occupied - species 04

  assign("initial01totmass", parameters$initial01totmass[simulnumb], pos = 1)   # initial total biomass - species 01
  assign("initial02totmass", parameters$initial02totmass[simulnumb], pos = 1)   # initial total biomass - species 02
  assign("initial03totmass", parameters$initial03totmass[simulnumb], pos = 1)   # initial total biomass - species 03
  assign("initial04totmass", parameters$initial04totmass[simulnumb], pos = 1)   # initial total biomass - species 04
  
  assign("maxrgr01", parameters$maxrgr01[simulnumb], pos = 1)    # maximum relative growth rate - species 01
  assign("maxrgr02", parameters$maxrgr02[simulnumb], pos = 1)    # maximum relative growth rate - species 02
  assign("maxrgr03", parameters$maxrgr03[simulnumb], pos = 1)    # maximum relative growth rate - species 03
  assign("maxrgr04", parameters$maxrgr04[simulnumb], pos = 1)    # maximum relative growth rate - species 04
  
  assign("overwinter01", parameters$overwinter01[simulnumb], pos = 1)    # proportion of individuals that overwinter - species 01  
  assign("overwinter02", parameters$overwinter02[simulnumb], pos = 1)    # proportion of individuals that overwinter - species 02
  assign("overwinter03", parameters$overwinter03[simulnumb], pos = 1)    # proportion of individuals that overwinter - species 03  
  assign("overwinter04", parameters$overwinter04[simulnumb], pos = 1)    # proportion of individuals that overwinter - species 04
  
  assign("halfsatB01", parameters$halfsatB01[simulnumb], pos = 1)    # biomass half saturation - species 01  
  assign("halfsatB02", parameters$halfsatB02[simulnumb], pos = 1)    # biomass half saturation - species 02
  assign("halfsatB03", parameters$halfsatB03[simulnumb], pos = 1)    # biomass half saturation - species 03  
  assign("halfsatB04", parameters$halfsatB04[simulnumb], pos = 1)    # biomass half saturation - species 04
  
  assign("halfsatN01", parameters$halfsatN01[simulnumb], pos = 1)    # nitrogen half saturation - species 01  
  assign("halfsatN02", parameters$halfsatN02[simulnumb], pos = 1)    # nitrogen half saturation - species 02
  assign("halfsatN03", parameters$halfsatN03[simulnumb], pos = 1)    # nitrogen half saturation - species 03  
  assign("halfsatN04", parameters$halfsatN04[simulnumb], pos = 1)    # nitrogen half saturation - species 04
  
  assign("halfsatP01", parameters$halfsatP01[simulnumb], pos = 1)    # phosphorus half saturation - species 01  
  assign("halfsatP02", parameters$halfsatP02[simulnumb], pos = 1)    # phosphorus half saturation - species 02
  assign("halfsatP03", parameters$halfsatP03[simulnumb], pos = 1)    # phosphorus half saturation - species 03  
  assign("halfsatP04", parameters$halfsatP04[simulnumb], pos = 1)    # phosphorus half saturation - species 04
  
  assign("uptakeN01", parameters$uptakeN01[simulnumb], pos = 1)    # nitrogen uptake rate - species 01  
  assign("uptakeN02", parameters$uptakeN02[simulnumb], pos = 1)    # nitrogen uptake rate - species 02
  assign("uptakeN03", parameters$uptakeN03[simulnumb], pos = 1)    # nitrogen uptake rate - species 03  
  assign("uptakeN04", parameters$uptakeN04[simulnumb], pos = 1)    # nitrogen uptake rate - species 04
  
  assign("uptakeP01", parameters$uptakeP01[simulnumb], pos = 1)    # phosphorus uptake rate - species 01  
  assign("uptakeP02", parameters$uptakeP02[simulnumb], pos = 1)    # phosphorus uptake rate - species 02
  assign("uptakeP03", parameters$uptakeP03[simulnumb], pos = 1)    # phosphorus uptake rate - species 03  
  assign("uptakeP04", parameters$uptakeP04[simulnumb], pos = 1)    # phosphorus uptake rate - species 04
}