#################################################################
# Makes a blank list of all of the elements (0s)                #
# Loop over all timesteps prior to running the simulation       #
# Then, START() initiates the first timestep - i.e., LIST[[1]]  #
#                                                               #
# Modified to work w/ new LIST structure - 1 species/matrix     #
#                                                               #
# Created by: Michael J. McCann - March 21, 2014                #
#################################################################

BLANK2 <- function(){  
    
  SPmatrix <- matrix(0, height, width)
  
  NEIGH <- matrix(0, height, width)
  
  TOTALP <- 0
  
  mylist <- c(rep(list(SPmatrix),numbspecies), list(NEIGH), list(TOTALP))
  
  # give each matrix in the list the right name 
  # the number of species matrices is variable, hence the paste and seq combo 
  names(mylist) <- c(paste("SP",seq(from=1,to=numbspecies,by=1),"matrix",sep=""), "NEIGH", "TOTALP")
  
  return(mylist)
}