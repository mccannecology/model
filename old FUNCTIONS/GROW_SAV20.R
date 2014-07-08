#######################################
# Growth function                     #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# Identical to Scheffer et al. 2003   #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$SAV                 #
# x2... LIST[[i]]$FPtotal             #
# x3... LIST[[i]]$TOTALP              #
# x4... LIST[[i]]$TOTALN              #
#                                     #
# Created: MJ McCann 3/23/2014        #
# Updated: 6/20/2014                  #
# No cap @ 100 g/m2                   #
#######################################
GROW_SAV20 <- function(x1,x2,x3,x4) { 
  
  new_SAV <- matrix(data=0,nrow=height,ncol=width)
  
  x1[x1>0] <- x1[x1>0] +  # initial biomass plus 

                (speciesmatrix$maxrgr[1]*x1[x1>0]) *  # new growth 
    
                # needs the corresponding cell in the FP matrix 
    
                (1/(1+(lightlimitation*x1[x1>0])+(shadingbyFP*x2[x1>0])+lightattenuation)) * # limitation according to Scheffer et al. 2003 
    
                (x4/(x4+speciesmatrix$halfsatN[1])) - # nutrient limitation 
    
                (lossSAV*x1[x1>0]) # biomass loss 
     
  new_SAV <- x1
  
  return(new_SAV) 
}



