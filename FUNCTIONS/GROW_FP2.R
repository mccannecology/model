#######################################
# Growth function                     #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$FPmatrix            #
# x2... LIST[[i+1]]$FPmatrix          #
# x3... LIST[[i]]$FPALLmatrix         #
# n...  numbFPspecies                 # 
# x4... LIST[[i]]$TOTALP              #
# x5... LIST[[i]]$TOTALN              #
#                                     #
# Created: MJ McCann 3/23/2014        #
# Updated: 5/9/2014                   #
# No cap @ 100 g/m2                   #
#######################################
GROW_FP2 <- function(x1,x2,x3,n,x4,x5) { 
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      if (x1[j,k] > 0) {
        x2[j,k] <- x1[j,k] + # initial biomass 
          
                      ((x1[j,k]*speciesmatrix$maxrgr[n+1]) * # new growth 
        
                      (x4/(x4+speciesmatrix$halfsatP[n+1])) * # phosphorus limitation 
          
                      (x5/(x5+speciesmatrix$halfsatN[n+1])) * # nitrogen limitation 
          
                      (x3[j,k]/(x3[j,k]+speciesmatrix$halfsatB[n+1])) - # biomass limitation 
          
                      (lossFP*x1[j,k])) # biomass loss 
      }
    }   
  }
  return(x2) 
}


