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
# load("testworkspace.Rdata") 
# two FP species
# load("testworkspace-2FPspecies.Rdata") 
# four FP species
# load("testworkspace-4FPspecies.Rdata") 
#########################################

MOVE21 <- function(x1,neigh_thresh=100,focal_thresh=0,amnt_colonize=1) { 
  
  require(raster)
  
  # Set-up biomass as raster objects 
  biomass <- raster(x1) # convert SAV matrix to a raster object 
  names(biomass) <- "biomass"
  
  # set up a new raster to hold the results 
  biomass_next <- biomass # convert SAV matrix to a raster object 
  names(biomass_next) <- "biomass_next"
  
  # define a matrix that looks at all nine neighbors, but not the focal cell 
  w <- matrix(1,3,3)
  w[2,2] <- 0
  
  # calculate the total biomass of neighbors and assign this to a raster 
  neighbor_biomass <- focal(biomass,w,pad=TRUE,padValue=0)
  names(neighbor_biomass) <- "neighbor_biomass"
  
  # view focal biomass and total neighbor biomass side by side 
  # plot(stack(biomass,neighbor_biomass))
  
  neighbor_biomass <- as.matrix(neighbor_biomass)
  biomass_next <- as.matrix(biomass_next)
  biomass <- as.matrix(biomass)
  
  # gives birth 
  # assign a small value to those cells that have low biomass AND 
  # that have lots of surrounding biomass 
  #biomass_next[Which(neighbor_biomass >= neigh_thresh) & Which(biomass <= focal_thresh)] <- (amnt_colonize + biomass)
  biomass_next[(neighbor_biomass >= neigh_thresh) & (biomass <= focal_thresh)] <- (amnt_colonize + biomass_next[(neighbor_biomass >= neigh_thresh) & (biomass <= focal_thresh)]) 
  # this is not right yet
  # if a cell is surrounded by 9 densely occupied cells it will get more than 
  # a cell occupied by 1 densely occupied cell 
  # Right-hand side of the equation above should be an equation 
  
  # view view current biomass, neighbor biomass, and biomass on next step 
  # plot(stack(biomass,neighbor_biomass,biomass_next))
  
  # substract off from cells in biomass that moved 
  # need to know the number of cells around them that are < threshold to move into 
  
  x1 <- as.matrix(biomass_next)
  
  return(x1)
}

