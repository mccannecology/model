#######################################
# Growth function                     #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$SPmatrix            #
# x2... LIST[[i+1]]$SPmatrix          #
# x3... LIST[[i]]$SPALLmatrix         #
# n... species # 
#                                     #
# Created: MJ McCann 3/23/2013        #
#######################################

GROW <- function(x1,x2,x3,n) { 
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      if (x1[j,k] > 0) {
        x2[j,k] <- x1[j,k] + speciesmatrix$maxrgr[n]*x1[j,k] * # initial biomass plus new growth 
                      
                      (speciesmatrix$halfsatB[n]/(x3[j,k]+speciesmatrix$halfsatB[n])) * # biomass limitation 
                      
                      ((TOTALP/(TOTALP+speciesmatrix$halfsatP[n]))*(TOTALN/(TOTALN+speciesmatrix$halfsatN[n]))) - # nutrient limitation 
          
                      (loss*x1[j,k]) # biomass loss 
      }  
    }   
  }
  return(x2) 
}


