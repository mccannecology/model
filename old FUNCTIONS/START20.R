##############################################################################################
# Assigns values to initial time step - e.g., LIST[[1]]                                      #    
#                                                                                            #
# Initiates the presence matrix and age matrix                                               #
# i.e., fills w/ initial number of individuals from each species (1s, 2s,... to n)           #
#                                                                                            #
# Modified to work w/ new LIST structure - 1 species/matrix                                  #
# Modified to work w/ SAV component                                                          # 
#                                                                                            #
# Created by: Michael J. McCann - March 21, 2014                                             #
# Updated: April 2014                                                                        # 
##############################################################################################

START20 <- function(x){ 
  
  # issue a warning message if you try to create too many initial individuals (i.e., more than # of spots)
  # if (sum(speciesmatrix$initial_cells) > (height*width)) {warning("Too many initial individuals for this pond!")}
  # This warning is turned off for now 
  # multiple species can occur in one cell, so total # cells occupied can exceed total area (height*width)
  
  ###########################################
  # Build presence/absence matrices for SAV # 
  ###########################################
  # assign the SAV cells 
  # SAV will always be the first row of the speciesmatrix 
  masspercell <- speciesmatrix[1,"initial_total_biomass"]/speciesmatrix[1,"initial_cells"]
  initial<-c(rep(0,((height*width)-speciesmatrix[1,"initial_cells"])),rep(masspercell,speciesmatrix[1,"initial_cells"])) # make a vector of 0s or #s depending on your initial_cell & initial_total_biomass # from the species matrix     
  x$SAV <- matrix(sample(initial),height,width) # randomly assign the elements of this vector to the matrix without replacement 
  
    
  #######################################################
  # Build presence/absence matrices for each FP species # 
  #######################################################
  for(i in 1:numbFPspecies){
    masspercell <- speciesmatrix[i+1,"initial_total_biomass"]/speciesmatrix[i+1,"initial_cells"]
    initial<-c(rep(0,((height*width)-speciesmatrix[i+1,"initial_cells"])),
               rep(masspercell,speciesmatrix[i+1,"initial_cells"])) 
    x$FP[[i]]<-matrix(sample(initial),height,width) # randomly assign the elements of this vector to the matrix without replacement  
  }  

  #############################################
  # Build FPALLmatrix - sum of all FP species #
  #############################################
  x$FPtotal <- Reduce('+', x$FP)
  
  #######################################
  # Initializing TOTAL N & TOTAL P 
  #######################################
  x$TOTALN <- TOTALN 
  
  x$TOTALP <- TOTALP 
  
  #######################################
  # Put it all together & return it 
  #######################################
  return(x)
}





