#######################################
# Growth function                     #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# Identical to Scheffer et al. 2003   #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$SAVmatrix           #
# x2... LIST[[i+1]]$SAVmatrix         #
# x3... LIST[[i]]$FPALLmatrix         #
# x4... LIST[[i]]$TOTALP              #
# x5... LIST[[i]]$TOTALN              #
#                                     #
# Created: MJ McCann 3/23/2014        #
# Updated: 6/20/2014                  #
# No cap @ 100 g/m2                   #
#######################################
GROW_SAV3 <- function(x1,x2,x3,x4,x5) { 
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      if (x1[j,k] > 0) {
        x2[j,k] <- x1[j,k] +  # initial biomass plus 

                      (speciesmatrix$maxrgr[1]*x1[j,k]) *  # new growth 
          
                      (1/(1+(lightlimitation*x1[j,k])+(shadingbyFP*x3[j,k])+lightattenuation)) * # limitation according to Scheffer et al. 2003 
          
                      (x5/(x5+speciesmatrix$halfsatN[1])) - # nutrient limitation 
          
                      (lossSAV*x1[j,k]) # biomass loss 
      }
    }   
  }
  return(x2) 
}


  

