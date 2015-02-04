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
# x3... LIST[[i]]$TOTALN              #
# x4... LAND                          #
#                                     #
# Created: MJ McCann 3/23/2014        #
# Updated: Sept 2014 - spatial nutr.  #
# No cap @ 100 g/m2                   #
#######################################
GROW_SAV30 <- function(x1,x2,x3,x4) { 
    
  new_SAV <- matrix(data=0,nrow=height1,ncol=width1)
  
  x1[x1>0 & x4==0] <- x1[x1>0 & x4==0] +  # initial biomass plus 

                (specieslist$maxrgr[1]*x1[x1>0 & x4==0]) *  # new growth 
    
                # needs the corresponding cell in the FP matrix 
    
                (1/(1+(specieslist$lightlimitation[1]*x1[x1>0 & x4==0])+(shadingbyFP*x2[x1>0 & x4==0])+lightattenuation)) * # limitation according to Scheffer et al. 2003 
    
                (x3[x1>0 & x4==0] / (x3[x1>0 & x4==0] + specieslist$halfsatN[1])) - # nutrient limitation 
    
                (lossSAV*x1[x1>0 & x4==0]) # biomass loss 
     
  new_SAV <- x1
  
  return(new_SAV) 
}

###########################
# Try with a real example #
###########################
