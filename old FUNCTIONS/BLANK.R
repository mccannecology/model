#################################################################
# Makes a blank list of all of the elements (0s)                #
# Loop over all timesteps prior to running the simulation       #
# Then, START() initiates the first timestep - i.e., LIST[[1]]  #
#                                                               #
# By: Michael J. McCann                                         #
# Last Updated: 2/26/2014                                       #
#################################################################

BLANK <- function(){  
  
  PAmatrix <- matrix(0, height, width) # Presence/Absence matrix
  
  AGEmatrix <- matrix(0, height, width) # Age matrix 
  
  NEIGH <- matrix(0, height, width) # Neighbor density matrix 
  
  RAND <- matrix(runif(height * width),height,width) # a random number generated for each cell in matrix  
  
  FAILEDlist <- NULL # list/vector to track individuals that failed to reproduce 
  
  # put it all together 
  return(list(PAmatrix=PAmatrix,AGEmatrix=AGEmatrix,NEIGH=NEIGH,RAND=RAND,FAILEDlist=FAILEDlist))
}