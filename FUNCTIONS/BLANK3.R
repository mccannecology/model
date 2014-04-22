#################################################################
# Makes a blank list of all of the elements (0s)                #
# Loop over all timesteps prior to running the simulation       #
# Then, START() initiates the first timestep - i.e., LIST[[1]]  #
#                                                               #
# Modified to work w/ new LIST structure - 1 species/matrix     #
# Modified to work w/ SAV component                             # 
#                                                               #
# Created by: Michael J. McCann - March 21, 2014                #
# Updated: April 2014                                           # 
#################################################################

BLANK3 <- function(){  
    
  SPmatrix <- matrix(0, height, width)
  
  TOTALN <- 0 
  
  TOTALP <- 0
  
  mylist <- c(rep(list(SPmatrix),numbFPspecies+2), list(TOTALN), list(TOTALP))
  
  # give each matrix in the list the right name 
  # the number of species matrices is variable, hence the paste and seq combo 
  names(mylist) <- c("SAVmatrix",paste("FP",seq(from=1,to=numbFPspecies,by=1),"matrix",sep=""), "FPALLmatrix","TOTALN", "TOTALP")
  
  return(mylist)
}