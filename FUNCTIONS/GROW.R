# x1... LIST[[i]]$SPmatrix 
# x2... LIST[[i+1]]$SPmatrix
# x3... LIST[[i]]$SPALLmatrix

GROW <- function(x1,x2,x3) { # function has LIST[[i]]$SPmatrix, LIST[[i+1]]$SPmatrix, and LIST[[i]]$SPALLmatrix as inputs 
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      if (x1[j,k] > 0) {
        x2[j,k] <- x1[j,k] + maxrgr*x1[j,k] * # initial biomass plus new growth 
                      
                      (halfsatB/(x3[j,k]+halfsatB)) * # biomass limitation 
                      
                      ((TOTALP/(TOTALP+halfsatP))*(TOTALN/(TOTALN+halfsatN))) - # nutrient limitation 
          
                      (loss*x1[j,k]) # biomass loss 
      }  
    }   
  }
  return(x2) 
}


