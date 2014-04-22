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

START5 <- function(){ 
  
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
    
  SAVmatrix <- matrix(sample(initial),height,width) # randomly assign the elements of this vector to the matrix without replacement 
  
  rm("masspercell","initial")
  
  #######################################################
  # Build presence/absence matrices for each FP species # 
  #######################################################
  # set-up a blank list 
  mylist<-list()
  
  for (n in 1:numbFPspecies) { 
    
    masspercell <- speciesmatrix[n+1,"initial_total_biomass"]/speciesmatrix[n+1,"initial_cells"]
  
    initial<-c(rep(0,((height*width)-speciesmatrix[n+1,"initial_cells"])),rep(masspercell,speciesmatrix[n+1,"initial_cells"])) # make a vector of 0s or #s depending on your initial_cell & initial_total_biomass # from the species matrix     
    
    mylist[[n]] <- matrix(sample(initial),height,width) # randomly assign the elements of this vector to the matrix without replacement 
  }
  
  #############################################
  # Build FPALLmatrix - sum of all FP species #
  #############################################
  FPALLmatrix <- matrix(0,height,width)  
  
  for (n in 1:numbFPspecies) {
     FPALLmatrix <- FPALLmatrix + mylist[[n]]
  }
  
  #######################################
  # Initializing TOTAL N & TOTAL P 
  #######################################
  TOTALN <- TOTALN 
  
  TOTALP <- TOTALP 
  
  #######################################
  # Put it all together & return it 
  #######################################
  mylist <- c(list(SAVmatrix), mylist, list(FPALLmatrix), list(TOTALN), list(TOTALP))
  
  names(mylist) <- c("SAVmatrix",paste("FP",seq(from=1,to=numbFPspecies,by=1),"matrix",sep=""),"FPALLmatrix","TOTALN","TOTALP")
  
  return(mylist)
}





