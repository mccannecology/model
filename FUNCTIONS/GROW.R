GROW <- function(x1,x2) { # function has both LIST[[i]]$SPmatrix and LIST[[i+1]]$SPmatrix as inputs 
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      if (x1[j,k] > 0) {
        x2[j,k] <- x1[j,k] + maxrgr*x1[j,k] * # initial biomass plus new growtht 
                      
                      (halfsatB/(x1[j,k]+halfsatB)) * # biomass limitation ** KEY CHANGE: replace x1[j,k] w/ a multispecies measure
                      
                      ((TOTALP/(TOTALP+halfsatP))*(TOTALN/(TOTALN+halfsatN))) - # nutrient limitation 
          
                      (loss*x1[j,k]) # biomass loss 
      }  
    }   
  }
  return(x2) 
}


