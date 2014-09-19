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
# Updated: September 2014 - spatially explicit nutrients                                     # 
##############################################################################################

START26 <- function(x){ 
  
  ###################### 
  # Define LAND matirx #
  ######################
  if(shape=="rectangle"){
    LAND <- matrix(0, height1, width1)
  }
  
  if(shape=="hook"){
    LAND <- matrix(0, height1, width1)
    LAND[1:height2,1:width2] <- 1 # top left cells equal 1 
  }
  
  if(shape=="eight"){
    LAND <- matrix(0, height1, width1)
    LAND[1:height2,1:width2] <- 1 # top left cells equal 1
    LAND[(height1-height2):height1,(width1-width2+1):width1] <- 1 # bottom right cells equal 1 
  }
  
  if(shape=="tee"){
    LAND <- matrix(0, height1, width1)
    LAND[1:height2,1:width2] <- 1  # top left cells equal 1
    LAND[1:height2,(width1-width2+1):width1] <- 1 # top right cells equal 1
  }
  
  if(shape=="cross"){
    LAND <- matrix(0, height1, width1)
    LAND[1:height2,1:width2] <- 1 # top left cells equal 1
    LAND[1:height2,(width1-width2+1):width1] <- 1 # top right cells equal 1
    LAND[(height1-height2):height1,(width1-width2+1):width1] <- 1 # bottom right cells equal 1 
    LAND[(height1-height2+1):height1,1:width2] <- 1 # bottom left cells equal 1
  }
   
  assign("LAND", LAND, pos = 1)  # assign LAND to the global environment 
  
  ###########################################
  # Build presence/absence matrices for SAV # 
  ###########################################
  # assign the SAV cells 
  # SAV will always be the first element in each vector in specieslist
  masspercell <- specieslist$initial_total_biomass[1]/specieslist$initial_cells[1]
 
  initial<-c(rep(0,((height1*width1)-specieslist$initial_cells[1]-sum(LAND))),
             
             rep(masspercell,specieslist$initial_cells[1])) # make a vector of 0s or #s depending on your initial_cell & initial_total_biomass # from the species matrix     
  
  x$SAV[LAND==0] <- sample(initial,(height1*width1-sum(LAND))) # randomly assign the elements of this vector to the matrix without replacement 
  
    
  #######################################################
  # Build presence/absence matrices for each FP species # 
  #######################################################
  for(i in 1:numbFPspecies){
    
    masspercell <- specieslist$initial_total_biomass[i+1] / specieslist$initial_cells[i+1]
    
    initial<-c(rep(0,((height1*width1)-specieslist$initial_cells[i+1]-sum(LAND))),
               
               rep(masspercell,specieslist$initial_cells[i+1])) 
    
    x$FP[[i]][LAND==0] <- sample(initial,(height1*width1-sum(LAND))) # randomly assign the elements of this vector to the matrix without replacement  
  }  

  #############################################
  # Build FPALLmatrix - sum of all FP species #
  #############################################
  x$FPtotal <- Reduce('+', x$FP)
  
  #######################################
  # Initializing TOTAL N & TOTAL P 
  #######################################
  # assign the single total N value to every cell in the grid 
  x$TOTALN <- matrix(0, height1, width1) # first make all of the cells (incl. LAND) 0
  x$TOTALN[LAND==0] <- TOTALN # then assign TOTALN to all cells that are not LAND 
  
  # assign the single total P value to every cell in the grid 
  x$TOTALP <- matrix(0, height1, width1) # first make all of the cells (incl. LAND) 0
  x$TOTALP[LAND==0] <- TOTALP # then assign TOTALP to all cells that are not LAND 
  
  #######################################
  # Put it all together & return it 
  #######################################
  return(x)
}





