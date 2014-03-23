##############################################################################################
# Assigns values to initial time step - e.g., LIST[[1]]                                      #    
#                                                                                            #
# Initiates the presence matrix and age matrix                                               #
# i.e., fills w/ initial number of individuals from each species (1s, 2s,... to n)           #
# Re-does the random number matrix for the first time step (probably not necessary)          #
# Leaves list of individuals that failed to reproduce blank                                  #
# Defines %ni% - negation of %in% - function to tell if something is not in a set            #
#                                                                                            #
# By: Michael J. McCann                                                                      #
# Last Updated: 2/26/2014                                                                    #
##############################################################################################

START3 <- function(){ 
  
  # issue a warning message if you try to create too many initial individuals (i.e., more than # of spots)
  if (sum(speciesmatrix$initial) > (height*width)) {warning("Too many initial individuals for this pond!")}
  
  #######################################
  # Build presence/absence matrix 
  #######################################
  # create vector (initial) of the number of 0s, 1s, 2s, etc. up to the number of species that you have 
  # initiate it with how many zeros you have 
  initial<-rep(0,(height*width)-sum(speciesmatrix$initial)) 
  
  # then add on the correct number of individuals from each species 
  for (n in 1:numbspecies) { 
    initial<-append(initial,rep(n,speciesmatrix$initial[n]))
  }
  
  # then randomly assign the elements of this vector to the PAmatrix without replacement 
  PAmatrix <- matrix(sample(initial),height,width)
  
  #######################################
  # Build age matrix 
  #######################################
  # need to change it so if PAmatrix is occupied... then I assign a 1 to the age matrix 
  AGEmatrix <- PAmatrix # Age matrix 
  AGEmatrix[AGEmatrix>1] <- 1
  
  #######################################
  # Build neighbor matrix 
  #######################################
  require(simecol)
  # define the number of cells from the focal cell that you want to count as neighbors
  wdist <- matrix(1,(2*buffer+1),(2*buffer+1)) # a matrix of 1s - size buffer around focal cell 
  wdist[buffer+1,buffer+1] <- 0 # change  focal cell to 0 - you don't want to count that as a neighbor
  NEIGH <- neighbours(PAmatrix, wdist = wdist, tol = 1, bounds = 0)  
  
  #######################################
  # Build random number matrix 
  #######################################
  RAND <- matrix(runif(height * width),height,width) # a random number generated for each cell in matrix  
  
  ###################################################################
  # Build list to keep track of individuals that failed to reproduce 
  ###################################################################
  FAILEDlist <- NULL 
  
  # put it all together 
  return(list(PAmatrix=PAmatrix,AGEmatrix=AGEmatrix,NEIGH=NEIGH,RAND=RAND,FAILEDlist=FAILEDlist))
}


# define a new operator - opposite of %in% - i.e., not in a subset
"%ni%" <- Negate("%in%") 


