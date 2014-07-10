#######################################
# Growth function                     #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# Identical to Scheffer et al. 2003   # 
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$FP                  #
# x2... LIST[[i]]$FPtotal             #
# i...  numbFPspecies                 # 
# x3... LIST[[i]]$TOTALP              #
# x4... LIST[[i]]$TOTALN              #
#                                     #
# Created: MJ McCann 3/23/2014        #
# Updated: 7/08/2014                  #
# No cap @ 100 g/m2                   #
#######################################
GROW_FP21 <- function(x1,x2,i,x3,x4) { 
  x1[[i]][x1[[i]]>0] <- x1[[i]][x1[[i]]>0] + # initial biomass 
    
                ((x1[[i]][x1[[i]]>0]*specieslist$maxrgr[i+1]) * # new growth 
        
                (x4/(x4+specieslist$halfsatN[i+1])) * # nitrogen limitation 
    
                (1/(1+specieslist$lightlimitation[i+1]*x2[x1[[i]]>0])) - # biomass limitation 
    
                (lossFP*x1[[i]][x1[[i]]>0])) # biomass loss 
  
  return(x1[[i]]) 
}

