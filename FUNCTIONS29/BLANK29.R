#################################################################
# Makes a blank list of all of the elements (0s)                #
# Loop over all timesteps prior to running the simulation       #
# Then, START() initiates the first timestep - i.e., LIST[[1]]  #
#                                                               #
# Modified to work w/ new LIST structure - 1 species/matrix     #
# Modified to work w/ SAV component                             # 
#                                                               #
# Created by: Michael J. McCann - March 21, 2014                #
# Updated: September 2014 - spatially explicit nutrients        # 
#################################################################
BLANK29 <- function(){  
    
  SPmatrix <- matrix(0, height1, width1)
  
  TOTALN <- matrix(0, height1, width1)
  
  TOTALP <- matrix(0, height1, width1)
  
  FP_Species<-rep(list(SPmatrix),numbFPspecies)
  
  mylist <- list(SPmatrix,FP_Species,SPmatrix,TOTALN, TOTALP)
  
  # give each matrix in the list the right name 
  # the number of species matrices is variable, hence the paste and seq combo 
  names(mylist) <- c("SAV","FP", "FPtotal","TOTALN", "TOTALP")
  
  return(mylist)
}