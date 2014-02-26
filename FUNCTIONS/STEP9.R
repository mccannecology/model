##################################################################################################
# Main function carried out on each time step                                                    #
# Includes: Reproduction, aging, senescence, overwinter, wind movement, etc.                     #
#                                                                                                #
# Need to add: species-specific density-dependence                                               #
# Need to add: individual variability in senescence (agedead)                                    #
#                                                                                                #
# Added: flexible # of species                                                                   #      
# Added: "coordinated movement with spilling" - all move in same direction, 1 row/col falls off  #  
# Added: overwintering                                                                           #
# Added: 2nd species                                                                             #
# Added: species-specific growth rate                                                            #
# Added: reproduction probability: based on RGR, density                                         #
# Added: species-specific senescence                                                             #
#                                                                                                #
# By: Michael J. McCann                                                                          #
# Last Updated: 2/26/2014                                                                        #
##################################################################################################

STEP9 <- function() { # start defining the function 
  
  for (i in 1:((timesteps+1)*years)) { # loop through time steps
    
    # check if it's NOT overwintering timestep, then do the following 
    if (i %ni% winters) {
      
      # move existing individuals to the next time step 
      LIST[[i+1]]$PAmatrix <- LIST[[i]]$PAmatrix  
      LIST[[i+1]]$AGEmatrix <- LIST[[i]]$AGEmatrix 
      
      # aging - only age those elements of AGEmatrix whose PAmatrix = 1 
      LIST[[i+1]]$AGEmatrix[LIST[[i]]$PAmatrix >= 1] <- LIST[[i]]$AGEmatrix[LIST[[i]]$PAmatrix >= 1] + 1 
      
      # senescence - loops over the different species 
      # requires a dataframe of species-specific variables - i.e., "speciesmatrix"
      for (n in 1:numbspecies) {
        LIST[[i+1]]$PAmatrix[LIST[[i]]$PAmatrix == n & LIST[[i]]$AGEmatrix >= speciesmatrix$agedead[n]] <- 0 
        LIST[[i+1]]$AGEmatrix[LIST[[i]]$PAmatrix == n & LIST[[i]]$AGEmatrix >= speciesmatrix$agedead[n]] <- 0
      }
      
      # define the number of cells from the focal cell that you want to count as neighbors
      # define wdist now because the reproduction steps (up next) use it 
      require(simecol)
      wdist <- matrix(1,(2*buffer+1),(2*buffer+1)) # a matrix of 1s - size buffer around focal cell 
      wdist[buffer+1,buffer+1] <- 0 # change  focal cell to 0 - you don't want to count that as a neighbor 
      
      # reproduction 
      for (j in 1:height) { # loop over all rows (height)
        for (k in 1:width) { # loop over all columns (width)
          
          # find occupied cells and see if they meet criteria to reproduce (growth rate, density-dependence)
          if (LIST[[i]]$PAmatrix[j,k] >= 1) {
            for (n in 1:numbspecies) {
              if ((LIST[[i]]$PAmatrix[j,k] == n & 
                     LIST[[i]]$RAND[j,k] < speciesmatrix$maxrgr[n]*(1 - LIST[[i]]$NEIGH[j,k]/sum(wdist)))){
                
                # if the cell is in the center, then do the following: 
                if ((j>=2 & j<= (height-1)) & (k>=2 & k<= (width-1))) { 
                
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j+distance, k, LIST[[i]]$PAmatrix[j+distance,k],
                                          j+distance, k+distance, LIST[[i]]$PAmatrix[j+distance,k+distance],
                                          j+distance, k-distance, LIST[[i]]$PAmatrix[j+distance,k-distance],
                                          j, k+distance, LIST[[i]]$PAmatrix[j,k+distance],
                                          j, k-distance, LIST[[i]]$PAmatrix[j,k-distance],
                                          j-distance, k, LIST[[i]]$PAmatrix[j-distance,k],
                                          j-distance, k-distance, LIST[[i]]$PAmatrix[j-distance,k-distance],
                                          j-distance, k+distance, LIST[[i]]$PAmatrix[j-distance,k+distance]),
                                        nrow=8,byrow=TRUE)
                    
                    # Remove coordinates that are outside the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # if neighbors is empty will need to add one to distance 
                    # maybe I do this after trying to reproduce 
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth: assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k]
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                  
                } # closes  if loop - through occupied individuals 
                
                # if the cell is in the top row (but not a corner), then do the following: 
                else if (j==1 & (k>=2 & k<= (width-1))) { 
                  
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j+distance, k, LIST[[i]]$PAmatrix[j+distance,k],
                                          j+distance, k+distance, LIST[[i]]$PAmatrix[j+distance,k+distance],
                                          j+distance, k-distance, LIST[[i]]$PAmatrix[j+distance,k-distance],
                                          j, k+distance, LIST[[i]]$PAmatrix[j,k+distance],
                                          j, k-distance, LIST[[i]]$PAmatrix[j,k-distance]),
                                        nrow=5,byrow=TRUE)
                    
                    # Remove coordinates that outside of the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # if neighbors is empty will need to add one to distance 
                    # maybe I do this after trying to reproduce 
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth - assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k] 
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                } # closes  if loop - through occupied individuals 
                
                # if the cell is in the bottom row (but not a corner), then do the following: 
                else if (j==height & (k>=2 & k<= (width-1))) { 
                  
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j, k+distance, LIST[[i]]$PAmatrix[j,k+distance],
                                          j, k-distance, LIST[[i]]$PAmatrix[j,k-distance],
                                          j-distance, k, LIST[[i]]$PAmatrix[j-distance,k],
                                          j-distance, k-distance, LIST[[i]]$PAmatrix[j-distance,k-distance],
                                          j-distance, k+distance, LIST[[i]]$PAmatrix[j-distance,k+distance]),
                                        nrow=5,byrow=TRUE)
                    
                    # Remove coordinates that outside of the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # if neighbors is empty will need to add one to distance 
                    # maybe I do this after trying to reproduce 
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth - assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k] 
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                } # closes  if loop - through occupied individuals 
                
                # if the cell is in the 1st column (but not a corner), then do the following: 
                else if (k==1 & (j>=2 & j<= (height-1))) { 
                  
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j+distance, k, LIST[[i]]$PAmatrix[j+distance,k],
                                          j+distance, k+distance, LIST[[i]]$PAmatrix[j+distance,k+distance],
                                          j, k+distance, LIST[[i]]$PAmatrix[j,k+distance],
                                          j-distance, k, LIST[[i]]$PAmatrix[j-distance,k],
                                          j-distance, k+distance, LIST[[i]]$PAmatrix[j-distance,k+distance]),
                                        nrow=5,byrow=TRUE)
                    
                    # Remove coordinates that outside of the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # if neighbors is empty will need to add one to distance 
                    # maybe I do this after trying to reproduce 
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth - assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k] 
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                } # closes  if loop - through occupied individuals 
                
                # if the cell is in the last column (but not a corner), then do the following: 
                else if (k==width & (j>=2 & j<= (height-1))) { 
                  
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j+distance, k, LIST[[i]]$PAmatrix[j+distance,k],
                                          j+distance, k-distance, LIST[[i]]$PAmatrix[j+distance,k-distance],
                                          j, k-distance, LIST[[i]]$PAmatrix[j,k-distance],
                                          j-distance, k, LIST[[i]]$PAmatrix[j-distance,k],
                                          j-distance, k-distance, LIST[[i]]$PAmatrix[j-distance,k-distance]),
                                        nrow=5,byrow=TRUE)
                    
                    # Remove coordinates that outside of the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # if neighbors is empty will need to add one to distance 
                    # maybe I do this after trying to reproduce 
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth - assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k] 
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                } # closes  if loop - through occupied individuals 
                
                # if the cell is in the top left corner, then do the following:  
                else if (j==1 & k==1) { 
                  
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j+distance, k, LIST[[i]]$PAmatrix[j+distance,k],
                                          j+distance, k+distance, LIST[[i]]$PAmatrix[j+distance,k+distance],                    
                                          j, k+distance, LIST[[i]]$PAmatrix[j,k+distance]),                                                
                                        nrow=3,byrow=TRUE)
                    
                    # Remove coordinates that outside of the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # if neighbors is empty will need to add one to distance 
                    # maybe I do this after trying to reproduce 
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth - assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k] 
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                } # closes  if loop - through occupied individuals 
                
                # if the cell is in the top right corner, then do the following:  
                else if (j==1 & k==width) { 
                  
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j+distance, k, LIST[[i]]$PAmatrix[j+distance,k],
                                          j+distance, k-distance, LIST[[i]]$PAmatrix[j+distance,k-distance],
                                          j, k-distance, LIST[[i]]$PAmatrix[j,k-distance]),                        
                                        nrow=3,byrow=TRUE)
                    
                    # Remove coordinates that outside of the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # if neighbors is empty will need to add one to distance 
                    # maybe I do this after trying to reproduce 
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth - assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k] 
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                } # closes  if loop - through occupied individuals 
                
                # if the cell is in the bottom left corner, then do the following:  
                else if (j==height & k==1) { 
                  
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j, k+distance, LIST[[i]]$PAmatrix[j,k+distance],
                                          j-distance, k, LIST[[i]]$PAmatrix[j-distance,k],
                                          j-distance, k+distance, LIST[[i]]$PAmatrix[j-distance,k+distance]),
                                        nrow=3,byrow=TRUE)
                    
                    # Remove coordinates that outside of the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # if neighbors is empty will need to add one to distance 
                    # maybe I do this after trying to reproduce 
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth - assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k] 
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                } # closes  if loop - through occupied individuals 
                
                # if the cell is in the bottom right corner, then do the following:  
                else if (j==height & k==width) { 
                  
                  STOP <- 0 # gets set to 1 when repeat loop should be broken (reproduction succesful) 
                  
                  distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
                  
                  # repeat loop
                  repeat{ # still need to close this around everything   
                    
                    distance <- distance+1 # adds one to your distance (how far away you try to reproduce) 
                    
                    # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
                    neighbors <- matrix(c(j, k-distance, LIST[[i]]$PAmatrix[j,k-distance],
                                          j-distance, k, LIST[[i]]$PAmatrix[j-distance,k],
                                          j-distance, k-distance, LIST[[i]]$PAmatrix[j-distance,k-distance]),
                                        nrow=3,byrow=TRUE)
                    
                    # Remove coordinates that outside of the grid (negatives or >width or width) or that are occupied 
                    neighbors <- subset(neighbors, 
                                        (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                        & (neighbors[,2]>=1 & neighbors[,2]<=width) 
                                        & neighbors[,3]==0)
                    
                    # randomly pick an empty neighbor to reproduce into 
                    offspring <- neighbors[sample(nrow(neighbors),1),]
                    
                    # give birth - assign 1 to that spot in the next time step 
                    LIST[[i+1]]$PAmatrix[offspring[1],offspring[2]] <- LIST[[i]]$PAmatrix[j,k] 
                    LIST[[i+1]]$AGEmatrix[offspring[1],offspring[2]] <-1
                    # end the repeat loop - reproduction was successful    
                    STOP <- 1 
                    
                    if(STOP == 1) break() # end the repeat loop when reproduction is successful 
                  } # closes the repeat loop 
                } # closes  if loop - through occupied individuals   
                
              } # closes if loop - through individuals that meet criteria for reproducing  
            } # closes for loop - through species  
          } # closes if loop - through occupied cells 
        } # closes for loop - through width/columns (k)    
      } # closes for loop - through height/rows (j) 
            
      # movement     
      RAND <- runif(1,0,1)
      if ("L" %in% wind_directions & RAND < wind_prob/length(wind_directions)) {
        # move: LEFT
        # removes first column and adds a column of zeros to the last column 
        LIST[[i+1]]$PAmatrix <- cbind(LIST[[i+1]]$PAmatrix[,-1],0)
        LIST[[i+1]]$AGEmatrix <- cbind(LIST[[i+1]]$AGEmatrix[,-1],0)
      }
      else if ("R" %in% wind_directions & RAND >= wind_prob/length(wind_directions) & RAND < 2*(wind_prob/length(wind_directions))) {
        # move: RIGHT
        # removes last column and adds a column of zeros to the first column 
        LIST[[i+1]]$PAmatrix <- cbind(0,LIST[[i+1]]$PAmatrix[,-width])
        LIST[[i+1]]$AGEmatrix <- cbind(0,LIST[[i+1]]$AGEmatrix[,-width])
      }
      else if("U" %in% wind_directions & RAND >= 2*(wind_prob/length(wind_directions)) & RAND < 3*(wind_prob/length(wind_directions))) {
        # move: UP
        # removes first row and adds a row of zeros to the last row 
        LIST[[i+1]]$PAmatrix <- rbind(LIST[[i+1]]$PAmatrix[-1,],0)
        LIST[[i+1]]$AGEmatrix <- rbind(LIST[[i+1]]$AGEmatrix[-1,],0)
      }
      else if("D" %in% wind_directions & RAND >= 3*(wind_prob/length(wind_directions))) {
        # move: DOWN
        # remove last row and adds a row of zeros to the first row 
        LIST[[i+1]]$PAmatrix <- rbind(0,LIST[[i+1]]$PAmatrix[-height,])
        LIST[[i+1]]$AGEmatrix <- rbind(0,LIST[[i+1]]$AGEmatrix[-height,])
      }
      
      # counts up neighbors after senescence, reproducing, and movement 
      LIST[[i+1]]$NEIGH <- neighbours(LIST[[i+1]]$PAmatrix, wdist = wdist, tol = 1, bounds = 0)  
      
    } # closes the if statement, when it is not an overwintering step 
    
    # otherwise, it IS an overwintering timestep, do the following
    else { 
      
      # move existing plants to the next time step 
      LIST[[i+1]]$PAmatrix <- LIST[[i]]$PAmatrix  
      LIST[[i+1]]$AGEmatrix <- LIST[[i]]$AGEmatrix 
      
      # This is turned OFF right now 
      # That means, individuals carry their age over from pre-overwintering
      # This prevents the "mass" cycles where lots of individuals die-off at the same time during the growing season 
      # reset age to 1  
      # LIST[[i+1]]$AGEmatrix[LIST[[i]]$PAmatrix >= 1] <- 1 
      
      # don't worry about those 1st two steps, I'm going to kill some plants now 
      
      # overwintering - loops over the different species 
      # requires a dataframe of species-specific variables - i.e., "speciesmatrix"
      for (n in 1:numbspecies) {      
        LIST[[i+1]]$PAmatrix[LIST[[i]]$PAmatrix == n & LIST[[i]]$RAND >= speciesmatrix$overwinter[n]] <- 0 
        LIST[[i+1]]$AGEmatrix[LIST[[i]]$PAmatrix == n & LIST[[i]]$RAND >= speciesmatrix$overwinter[n]] <- 0 
      }
      
      # counts up neighbors after overwintering
      LIST[[i+1]]$NEIGH <- neighbours(LIST[[i+1]]$PAmatrix, wdist = wdist, tol = 1, bounds = 0)  
      
    } # closes the else statement - when it is an overwintering step
    
    require(raster)
    plot(raster(LIST[[i]]$PAmatrix),main=i)
    
  } # closes for loop - through time(i)
  
  return(LIST)
    
} # closes function    