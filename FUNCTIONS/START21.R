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
# Updated: 07/08/2014                                                                        # 
##############################################################################################

START21 <- function(x){ 
  
  ###########################################
  # Build presence/absence matrices for SAV # 
  ###########################################
  # assign the SAV cells 
  # SAV will always be the first element in each vector in specieslist
  masspercell <- specieslist$initial_total_biomass[1]/specieslist$initial_cells[1]
 
  initial<-c(rep(0,((height*width)-specieslist$initial_cells[1])),rep(masspercell,specieslist$initial_cells[1])) # make a vector of 0s or #s depending on your initial_cell & initial_total_biomass # from the species matrix     
  
  x$SAV <- matrix(sample(initial),height,width) # randomly assign the elements of this vector to the matrix without replacement 
  
    
  #######################################################
  # Build presence/absence matrices for each FP species # 
  #######################################################
  for(i in 1:numbFPspecies){
    
    masspercell <- specieslist$initial_total_biomass[i+1] / specieslist$initial_cells[i+1]
    
    initial<-c(rep(0,((height*width)-specieslist$initial_cells[i+1])),
               
               rep(masspercell,specieslist$initial_cells[i+1])) 
    
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





