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
  if (sum(speciesmatrix$initial_cells) > (height*width)) {warning("Too many initial individuals for this pond!")}
  
  ####################################################
  # Build presence/absence matrices for each species 
  ####################################################
  # loop through # of species you have 
  mylist<-list()
  
  for (n in 1:numbspecies) { 
    
    masspercell <- speciesmatrix[n,"initial_total_biomass"]/speciesmatrix[n,"initial_cells"]
  
    initial<-c(rep(0,((height*width)-speciesmatrix[n,"initial_cells"])),rep(masspercell,speciesmatrix[n,"initial_cells"])) # make a vector of 0s or #s depending on your initial_cell & initial_total_biomass # from the species matrix     
    
    mylist[[n]] <- matrix(sample(initial),height,width) # randomly assign the elements of this vector to the SPmatrix without replacement 
  }
  
  ####################################################
  # Build SPALLmatrix - sum of all species 
  ####################################################
  SPALLmatrix <- matrix(0,height,width)  
  
  for (n in 1:numbspecies) {
    SPALLmatrix <- SPALLmatrix + mylist[[n]]
  }
  
  #######################################
  # Initializing TOTAL N & TOTAL P 
  #######################################
  TOTALN <- TOTALN 
  
  TOTALP <- TOTALP 
  
  #######################################
  # Put it all together & return it 
  #######################################
  mylist <- c(mylist, list(SPALLmatrix), list(TOTALN), list(TOTALP))
  
  names(mylist) <- c(paste("SP",seq(from=1,to=numbspecies,by=1),"matrix",sep=""),"SPALLmatrix","TOTALN","TOTALP")
  
  return(mylist)
}





