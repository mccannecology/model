##############################################################################################
# Assigns values to initial time step - e.g., LIST[[1]]                                      #    
#                                                                                            #
# Initiates the presence matrix and age matrix                                               #
# i.e., fills w/ initial number of individuals from each species (1s, 2s,... to n)           #
#                                                                                            #
# Modified to work w/ new LIST structure - 1 species/matrix                                  #
#                                                                                            #
# Created by: Michael J. McCann - March 21, 2014                                             #
##############################################################################################

START4 <- function(){ 
  
  # issue a warning message if you try to create too many initial individuals (i.e., more than # of spots)
  if (sum(speciesmatrix$initial) > (height*width)) {warning("Too many initial individuals for this pond!")}
  
  #######################################
  # Build presence/absence matrix 
  #######################################
  # loop through # of species you have 
  mylist<-list()
  
  for (n in 1:numbspecies) { 
    initial<-c(rep(0,((height*width)-speciesmatrix[n,"initial"])),rep(1,speciesmatrix[n,"initial"])) # make a vector of 0s or 1s depending on your initial # from the species matrix     
    mylist[[n]] <- matrix(sample(initial),height,width) # randomly assign the elements of this vector to the SPmatrix without replacement 
  }
  
  #######################################
  # Build neighbor matrix 
  #######################################
  # require(simecol)
  # define the number of cells from the focal cell that you want to count as neighbors
  # wdist <- matrix(1,(2*buffer+1),(2*buffer+1)) # a matrix of 1s - size buffer around focal cell 
  # wdist[buffer+1,buffer+1] <- 0 # change  focal cell to 0 - you don't want to count that as a neighbor
  # NEIGH <- neighbours(PAmatrix, wdist = wdist, tol = 1, bounds = 0)  
  
  #######################################
  # Initializing TOTAL N & TOTAL P 
  #######################################
  TOTALN <- 2 
  
  TOTALP <- 0.5
  
  #######################################
  # Put it all together & return it 
  #######################################
  mylist <- c(mylist, list(TOTALN), list(TOTALP))
  
  names(mylist) <- c(paste("SP",seq(from=1,to=numbspecies,by=1),"matrix",sep=""),"TOTALN","TOTALP")
  
  return(mylist)
}





