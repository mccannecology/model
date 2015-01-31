#######################################
# Movement function                   #
# cells surrounded by lots of biomass #
# get colonized                       #
#                                     # 
# INPUTS:                             #
# x1... LIST[[i]]$SAV or LIST[[i]]$FP #
#                                     #
# Created: MJ McCann 7/06/2014        #
# Updated: 7/8/2014                   # 
#######################################

#########################################
# load workspace for de-bugging 
# LIST has an initial time step
# short (3 yrs, 50 days each)
# one FP species
# load("testworkspace-1FPspecies.Rdata") 
# four FP species
# load("testworkspace-4FPspecies.Rdata") 
#########################################

MOVE29 <- function(x1,neigh_thresh=100,focal_thresh=0,amnt_colonize=1) { 
  
  # biomass... biomass on current time step 
  # biomass_next... biomass on next time step 
  # neighbor_biomass... 
  
  require(raster)
  
  # Set-up biomass as raster objects 
  biomass <- raster(x1) # convert biomass matrix to a raster object 
  names(biomass) <- "biomass"
  
  # set up a new raster to hold the results 
  biomass_next <- biomass 
  names(biomass_next) <- "biomass_next"
  
  # define a matrix that looks at all nine neighbors, but not the focal cell 
  w <- matrix(1,3,3)
  w[2,2] <- 0
  
  # calculate the total biomass of neighbors and assign this to a raster 
  neighbor_biomass <- focal(biomass,w,pad=TRUE,padValue=0)
  names(neighbor_biomass) <- "neighbor_biomass"
  
  # view focal biomass and total neighbor biomass side by side 
  # plot(stack(biomass,neighbor_biomass))
  
  # convert raster layers to matrices 
  neighbor_biomass <- as.matrix(neighbor_biomass)
  biomass_next <- as.matrix(biomass_next)
  biomass <- as.matrix(biomass)
  
  # set neighbor_biomass of any cells that are land to 0 
  neighbor_biomass[LAND==1] <- 0 
  
  # colonize 
  # assign (amnt_colonize) to  cells <focal_thresh & neigh_biomass > neigh_thresh 
  biomass_next[(neighbor_biomass >= neigh_thresh) & (biomass <= focal_thresh)] <- (amnt_colonize + biomass_next[(neighbor_biomass >= neigh_thresh) & (biomass <= focal_thresh)]) 
  
  # number of new colonizers   
  numb_colonizers <- length(biomass_next[(neighbor_biomass >= neigh_thresh) & (biomass <= focal_thresh)])
  
  # amount to subtract   
  amnt_subtract <- amnt_colonize*(numb_colonizers/length(biomass_next[biomass >= neigh_thresh/2]))
  
  # sutract new biomass from existing cells 
  # subtract from cells above neigh_thresh/2 (cells that are likely origins of colonizers)
  # Amount to subtract is the amnt_colonize * # of newly colonized cells
  # divided by the number of cells above neigh_thresh/2 
  biomass_next[biomass >= neigh_thresh/2] <- (biomass_next[biomass >= neigh_thresh/2] - amnt_subtract)
  
  # view view current biomass, neighbor biomass, and biomass on next step 
  # plot(stack(biomass,neighbor_biomass,biomass_next))
  
  # UNRESOLVED: 
  # substract off from cells in biomass that moved 
  # need to know the number of cells around them that are < threshold to move into 
  
  x1 <- as.matrix(biomass_next)
  
  return(x1)
}


# test with real matrix
# temp <- MOVE25(LIST[[1]]$SAV,neigh_thresh_SAV,focal_thresh_SAV,amnt_colonize_SAV)



