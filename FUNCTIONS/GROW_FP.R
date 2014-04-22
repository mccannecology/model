#######################################
# Growth function                     #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$FPmatrix            #
# x2... LIST[[i+1]]$FPmatrix          #
# x3... LIST[[i]]$FPALLmatrix         #
# n...  numbFPspecies                 # 
# x4... LIST[[i]]$TOTALP              #
# x5... LIST[[i]]$TOTALN              #
#                                     #
# Created: MJ McCann 3/23/2013        #
#######################################
GROW_FP <- function(x1,x2,x3,n,x4,x5) { 
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      if (x1[j,k] > 0) {
        x2[j,k] <- x1[j,k] + ((1-(x3[j,k]/100))*speciesmatrix$maxrgr[n+1])*x1[j,k] * # initial biomass plus new growth 

                      # the growth rate limitation by biomass is completely linear
                      # decrease from maxrgr @ biomass = 0
                      # to no growth @ biomass = 100
                      # this should cap the biomass near 100 
                      # Ideally, this would be modified to be a Monod (non-linear)
          
                      ((x4/(x4+speciesmatrix$halfsatP[n+1]))*(x5/(x5+speciesmatrix$halfsatN[n+1]))) - # nutrient limitation 
          
                      (loss*x1[j,k]) # biomass loss 
      }
    }   
  }
  return(x2) 
}


