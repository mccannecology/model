require(raster)

# Set-up biomass as raster objects 
biomass <- raster(LIST[[1]]$SAV) # convert SAV matrix to a raster object 
names(biomass) <- "biomass"

# set up a new raster to hold the results 
biomass_next <- biomass # convert SAV matrix to a raster object 
names(biomass_next) <- "biomass_next"

# define a matrix that looks at all nine neighbors, but not the focal cell 
w <- matrix(1,3,3)
w[2,2] <- 0
w

# calculate the total biomass of neighbors and assign this to a raster 
neighbor_biomass <- focal(biomass,w,pad=TRUE,padValue=0)
names(neighbor_biomass) <- "neighbor_biomass"

# view focal biomass and total neighbor biomass side by side 
plot(stack(biomass,neighbor_biomass))

# gives birth 
# assign a small value to those cells that have low biomass AND 
# that have lots of surrounding biomass 
biomass_next[Which(neighbor_biomass >= 100) & Which(biomass < 100)] <- 1
# this is not right yet
# if a cell is surrounded by 9 densely occupied cells it will get more than 
# a cell occupied by 1 densely occupied cell 
# Right-hand side of the equation above should be an equation 

# view view current biomass, neighbor biomass, and biomass on next step 
plot(stack(biomass,neighbor_biomass,biomass_next))

# substract off from cells in biomass that moved 
# need to know the number of cells around them that are < threshold to move into 


