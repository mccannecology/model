STEP10 <- function() { # start defining the function 
  
  for (i in 1:((timesteps+1)*years)) { # loop through time steps
    
    # check if it's NOT overwintering timestep, then do the following 
    if (i %ni% winters) { 
    
      if (numbspecies == 4) {
        LIST[[i+1]]$SP4matrix <- GROW()  
        LIST[[i+1]]$SP3matrix <- GROW()  
        LIST[[i+1]]$SP2matrix <- GROW() 
        LIST[[i+1]]$SP1matrix <- GROW()  
      }
      else if (numbspecies == 3) {
        LIST[[i+1]]$SP3matrix <- GROW()  
        LIST[[i+1]]$SP2matrix <- GROW() 
        LIST[[i+1]]$SP1matrix <- GROW() 
      }
      else if (numbspecies == 2) {
        LIST[[i+1]]$SP2matrix <- GROW() 
        LIST[[i+1]]$SP1matrix <- GROW() 
      }
      else if (numbspecies == 1) {
        LIST[[i+1]]$SP1matrix <- GROW() 
      }
    
      
    } # closes the if statement, when it is not an overwintering step 
    
    # otherwise, it IS an overwintering timestep, do the following
    else {   
      
    } # closes the else statement - when it is an overwintering step
  }
}

# define a new operator - opposite of %in% - i.e., not in a subset
"%ni%" <- Negate("%in%") 