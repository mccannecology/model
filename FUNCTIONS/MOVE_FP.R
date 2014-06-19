#######################################
# Growth function                     #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$FPmatrix            #
#                                     #
# Created: MJ McCann 3/22/2014        #
# Updated: 4/2014                     #
#######################################
MOVE_FP <- function(x1) { 
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      
      # find cells that meet criteria to move (> threshold)
      if (x1[j,k] >= minthresholdtomoveFP){

        # if the cell is in the center, then do the following: 
        if ((j>=2 & j<= (height-1)) & (k>=2 & k<= (width-1))) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j+distance, k, 
                                  j+distance, k+distance, 
                                  j+distance, k-distance, 
                                  j, k+distance, 
                                  j, k-distance, 
                                  j-distance, k, 
                                  j-distance, k-distance, 
                                  j-distance, k+distance),
                                nrow=8,byrow=TRUE)
            
            # Remove coordinates that are outside the grid (negatives or >width or width) 
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))
                         
            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
           
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
          
        } # closes  if loop - through occupied center cells 
        
        # if the cell is in the top row (but not a corner), then do the following: 
        else if (j==1 & (k>=2 & k<= (width-1))) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j+distance, k,
                                  j+distance, k+distance,
                                  j+distance, k-distance,
                                  j, k+distance,
                                  j, k-distance),
                                nrow=5,byrow=TRUE)
            
            # Remove neighbor coordinates that are outside the grid (negatives or >width or width)
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))
                        
            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
            
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
        } # closes  if loop - through occupied top row 
        
        # if the cell is in the bottom row (but not a corner), then do the following: 
        else if (j==height & (k>=2 & k<= (width-1))) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j, k+distance,
                                  j, k-distance,
                                  j-distance, k,
                                  j-distance, k-distance,
                                  j-distance, k+distance),
                                nrow=5,byrow=TRUE)
            
            # Remove neighbor coordinates that are outside the grid (negatives or >width or width)
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))
            
            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
            
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
        } # closes  if loop - through occupied bottom row 
        
        # if the cell is in the 1st column (but not a corner), then do the following: 
        else if (k==1 & (j>=2 & j<= (height-1))) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j+distance, k,
                                  j+distance, k+distance,
                                  j, k+distance,
                                  j-distance, k,
                                  j-distance, k+distance),
                                nrow=5,byrow=TRUE)
            
            # Remove coordinates that outside of the grid (negatives or >width or width)
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))

            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
            
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
        } # closes  if loop - through occupied 1st column  
        
        # if the cell is in the last column (but not a corner), then do the following: 
        else if (k==width & (j>=2 & j<= (height-1))) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j+distance, k,
                                  j+distance, k-distance,
                                  j, k-distance,
                                  j-distance, k,
                                  j-distance, k-distance),
                                nrow=5,byrow=TRUE)
            
            # Remove neighbor coordinates that are outside the grid (negatives or >width or width)
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))
             
            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
            
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
        } # closes  if loop - through occupied last column 
        
        # if the cell is in the top left corner, then do the following:  
        else if (j==1 & k==1) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j+distance, k,
                                  j+distance, k+distance,                    
                                  j, k+distance),                                                
                                nrow=3,byrow=TRUE)
            
            # Remove neighbor coordinates that are outside the grid (negatives or >width or width)
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))
                  
            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
            
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
        } # closes  if loop - through occupied top left
        
        # if the cell is in the top right corner, then do the following:  
        else if (j==1 & k==width) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j+distance, k,
                                  j+distance, k-distance,
                                  j, k-distance),                        
                                nrow=3,byrow=TRUE)
            
            # Remove neighbor coordinates that are outside the grid (negatives or >width or width)
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))

            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
            
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
        } # closes  if loop - through occupied top right  
        
        # if the cell is in the bottom left corner, then do the following:  
        else if (j==height & k==1) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j, k+distance,
                                  j-distance, k,
                                  j-distance, k+distance),
                                nrow=3,byrow=TRUE)
            
            # Remove neighbor coordinates that are outside the grid (negatives or >width or width)
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))

            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
            
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
        } # closes  if loop - through occupied bottom left 
        
        # if the cell is in the bottom right corner, then do the following:  
        else if (j==height & k==width) { 
          
          STOP <- 0 # gets set to 1 when repeat loop should be broken (movement succesful) 
          
          distance <- 0 # add 1 to this on each try of repeat{} - try a little bit further each time 
          
          # repeat loop
          repeat{ # still need to close this around everything   
            
            distance <- distance+1 # adds one to your distance (how far away you try to move) 
            
            # Returns an 8x3 matrix of the indices and occupancy of your 8 neighbors 
            neighbors <- matrix(c(j, k-distance,
                                  j-distance, k,
                                  j-distance, k-distance),
                                nrow=3,byrow=TRUE)
            
            # Remove neighbor coordinates that are outside the grid (negatives or >width or width)
            neighbors <- subset(neighbors, 
                                (neighbors[,1]>=1 & neighbors[,1]<=height) 
                                & (neighbors[,2]>=1 & neighbors[,2]<=width))
            
            # randomly pick a neighbor to move into 
            offspring <- neighbors[sample(nrow(neighbors),1),]
            
            # move: cells > minthresholdtomoveFP but < maxthresholdtomoveFP + maxamounttomoveFP
            if (x1[j,k] < maxthresholdtomoveFP + maxamounttomoveFP) {
              amounttomove <- (maxamounttomoveFP/(maxthresholdtomoveFP-minthresholdtomoveFP))*(x1[j,k]-minthresholdtomoveFP)
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot  
            }
            
            # move: cells > maxthresholdtomoveFP + maxamounttomove
            else {
              amounttomove <- x1[j,k] - maxthresholdtomoveFP # bring cells greater than 100 down to 100 
              x1[offspring[1],offspring[2]] <- x1[offspring[1],offspring[2]] + amounttomove # add this value to the new spot  
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
            
            # end the repeat loop - movement was successful    
            STOP <- 1 
            
            if(STOP == 1) break() # end the repeat loop when movement is successful 
          } # closes the repeat loop 
        } # closes  if loop - through occupied bottom right  
            
      } # closes if loop - through occupied cells 
    } # closes for loop - through width/columns (k)    
  } # closes for loop - through height/rows (j) 
  return(x1)
}