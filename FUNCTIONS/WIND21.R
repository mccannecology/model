#######################################
# Wind-movement function              #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$FPmatrix            #
#                                     #
# Created: MJ McCann 5/8/2014         #
# Updated: 07/08/2014
#######################################

WIND20 <- function (x1,proptomove,direction_of_wind,full_threshold=800) {
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      
      # find cells that are occupied 
      if (x1[j,k] > 0) {
        
        # if the cell is in the center, then do the following: 
        if ((j>=2 & j<=(height-1)) & (k>=2 & k<=(width-1))) {                                           
                
          amounttomove <- x1[j,k]*proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j-1,k] <- x1[j-1,k] + amounttomove # add amountomove UP
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_threshold) { # only move if the destination cell is not "too full" 
            x1[j,k+1] <- x1[j,k+1] + amounttomove # add amountomove RIGHT
            x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j+1,k] <- x1[j+1,k] + amounttomove # add amountomove DOWN
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k-1] <- x1[j,k-1] + amounttomove # add amountomove LEFT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          
        } # closes  if loop - through occupied center cells 
        
        # if the cell is in the top row (but not a corner), then do the following: 
        else if (j==1 & (k>=2 & k<=(width-1))) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){       
            x1[j,k] <- x1[j,k] 
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k+1] <- x1[j,k+1] + amounttomove # add amountomove RIGHT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j+1,k] <- x1[j+1,k] + amounttomove # add amountomove DOWN
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k-1] <- x1[j,k-1] + amounttomove # add amountomove LEFT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
        } # closes  if loop - through occupied top row 
        
        # if the cell is in the bottom row (but not a corner), then do the following: 
        else if (j==height & (k>=2 & k<=(width-1))) {           
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j-1,k] <- x1[j-1,k] + amounttomove # add amountomove UP
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k+1] <- x1[j,k+1] + amounttomove # add amountomove RIGHT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){       
            x1[j,k] <- x1[j,k] 
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k-1] <- x1[j,k-1] + amounttomove # add amountomove LEFT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
        } # closes  if loop - through occupied bottom row 
        
        # if the cell is in the 1st column (but not a corner), then do the following: 
        else if (k==1 & (j>=2 & j<=(height-1))) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j-1,k] <- x1[j-1,k] + amounttomove # add amountomove UP
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k+1] <- x1[j,k+1] + amounttomove # add amountomove RIGHT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j+1,k] <- x1[j+1,k] + amounttomove # add amountomove DOWN
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){       
            x1[j,k] <- x1[j,k] 
          }
        } # closes  if loop - through occupied 1st column  
        
        # if the cell is in the last column (but not a corner), then do the following: 
        else if (k==width & (j>=2 & j<=(height-1))) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j-1,k] <- x1[j-1,k] + amounttomove # add amountomove UP
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){       
            x1[j,k] <- x1[j,k] 
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j+1,k] <- x1[j+1,k] + amounttomove # add amountomove DOWN
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k-1] <- x1[j,k-1] + amounttomove # add amountomove LEFT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
        } # closes  if loop - through occupied last column 
        
        # if the cell is in the top left corner, then do the following:  
        else if (j==1 & k==1) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){        
            x1[j,k] <- x1[j,k] 
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k+1] <- x1[j,k+1] + amounttomove # add amountomove RIGHT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j+1,k] <- x1[j+1,k] + amounttomove # add amountomove DOWN
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){       
            x1[j,k] <- x1[j,k] 
          }  
        } # closes  if loop - through occupied top left
        
        # if the cell is in the top right corner, then do the following:  
        else if (j==1 & k==width) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){       
            x1[j,k] <- x1[j,k] 
          }
          if (direction_of_wind == "R"){       
            x1[j,k] <- x1[j,k] 
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j+1,k] <- x1[j+1,k] + amounttomove # add amountomove DOWN
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k-1] <- x1[j,k-1] + amounttomove # add amountomove LEFT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          
        } # closes  if loop - through occupied top right  
        
        # if the cell is in the bottom left corner, then do the following:  
        else if (j==height & k==1) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j-1,k] <- x1[j-1,k] + amounttomove # add amountomove UP
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k+1] <- x1[j,k+1] + amounttomove # add amountomove RIGHT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          } 
          if (direction_of_wind == "D"){      
            x1[j,k] <- x1[j,k] 
          }
          if (direction_of_wind == "L"){       
            x1[j,k] <- x1[j,k] 
          }
          
        } # closes  if loop - through occupied bottom left 
        
        # if the cell is in the bottom right corner, then do the following:  
        else if (j==height & k==width) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j-1,k] <- x1[j-1,k] + amounttomove # add amountomove UP
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){       
            x1[j,k] <- x1[j,k]
          }
          if (direction_of_wind == "D"){      
            x1[j,k] <- x1[j,k] 
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_threshold) { # only move if the destination cell is not "too full" 
              x1[j,k-1] <- x1[j,k-1] + amounttomove # add amountomove LEFT
              x1[j,k] <- x1[j,k] - amounttomove # subtract this value from the original spot 
            }
          }
        } # closes  if loop - through occupied bottom right  
        
      } # closes if loop - through occupied cells 
    } # closes for loop - through width/columns (k)    
  } # closes for loop - through height/rows (j)  
  return(x1)
}