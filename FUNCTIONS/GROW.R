GROW <- function(x1,x2) { # function has both LIST[[i]]$SPmatrix and LIST[[i+1]]$SPmatrix as inputs 
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      if (x1[j,k] > 0) {
        x2[j,k] <- x1[j,k] + 1 # This is where the growth formula goes 
      }  
    }   
  }
  return(x2) 
}


