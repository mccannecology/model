#######################################
# Wind-movement function              #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$FPmatrix            #
#                                     #
# Created: MJ McCann 5/8/2014         #
# Updated: 07/08/2014                 #
#######################################

WIND22 <- function (x1,proptomove,direction_of_wind,full_thresh_wind=800) {
  x2 <- x1 # this will hold the values for the next time step 
  
  # new mass originating from center cells 
  new_mass_C <- matrix(0,nrow=height,ncol=width)
  remove_mass_C <- matrix(0,nrow=height,ncol=width)
  
  # new mass originating from top row (blank matrix for now)
  new_mass_Trow <- new_mass_C
  remove_mass_Trow <- new_mass_C

  # new mass originating from bottom row (blank matrix for now)
  new_mass_Brow <- new_mass_C
  remove_mass_Brow <- new_mass_C
  
  # new mass originating from first column (blank matrix for now) 
  new_mass_Fcol <- new_mass_C
  remove_mass_Fcol <- new_mass_C
  
  # new mass originating from last column (blank matrix for now) 
  new_mass_Lcol <- new_mass_C
  remove_mass_Lcol <- new_mass_C
  
  # new mass originating from top left (blank matrix for now) 
  new_mass_TL <- new_mass_C
  remove_mass_TL <- new_mass_C
  
  # new mass originating from top right (blank matrix for now) 
  new_mass_TR <- new_mass_C
  remove_mass_TR <- new_mass_C
  
  # new mass originating from bottom left (blank matrix for now) 
  new_mass_BL <- new_mass_C
  remove_mass_BL <- new_mass_C
  
  # new mass originating from bottom right (blank matrix for now) 
  new_mass_BR <- new_mass_C
  remove_mass_BR <- new_mass_C
  
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      
      # find cells that are occupied 
      if (x1[j,k] > 0) {
        
        # if the cell is in the center, then do the following: 
        if ((j>=2 & j<=(height-1)) & (k>=2 & k<=(width-1))) {                                           
          
          amounttomove <- x1[j,k]*proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_C[j-1,k] <- amounttomove # add amountomove UP
              remove_mass_C[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_C[j,k+1] <- amounttomove # add amountomove RIGHT
              remove_mass_C[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_C[j+1,k] <- amounttomove # add amountomove DOWN
              remove_mass_C[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_C[j,k-1] <- amounttomove # add amountomove LEFT
              remove_mass_C[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          
        } # closes  if loop - through occupied center cells 
        
        # if the cell is in the top row (but not a corner), then do the following: 
        else if (j==1 & (k>=2 & k<=(width-1))) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){       
            new_mass_Trow[j,k] <- 0 
            remove_mass_Trow[j,k] <- 0
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Trow[j,k+1] <- amounttomove # add amountomove RIGHT
              remove_mass_Trow[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Trow[j+1,k] <- amounttomove # add amountomove DOWN
              remove_mass_Trow[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Trow[j,k-1] <- amounttomove # add amountomove LEFT
              remove_mass_Trow[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
        } # closes  if loop - through occupied top row 
        
        # if the cell is in the bottom row (but not a corner), then do the following: 
        else if (j==height & (k>=2 & k<=(width-1))) {           
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Brow[j-1,k] <- amounttomove # add amountomove UP
              remove_mass_Brow[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Brow[j,k+1] <- amounttomove # add amountomove RIGHT
              remove_mass_Brow[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){       
            new_mass_Brow[j,k] <- 0 
            remove_mass_Brow[j,k] <- 0
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Brow[j,k-1] <- amounttomove # add amountomove LEFT
              remove_mass_Brow[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
        } # closes  if loop - through occupied bottom row 
        
        # if the cell is in the 1st column (but not a corner), then do the following: 
        else if (k==1 & (j>=2 & j<=(height-1))) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Fcol[j-1,k] <- amounttomove # add amountomove UP
              remove_mass_Fcol[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Fcol[j,k+1] <- amounttomove # add amountomove RIGHT
              remove_mass_Fcol[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Fcol[j+1,k] <- amounttomove # add amountomove DOWN
              remove_mass_Fcol[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){       
            new_mass_Fcol[j,k] <- 0 
            remove_mass_Fcol[j,k] <- 0
          }
        } # closes  if loop - through occupied 1st column  
        
        # if the cell is in the last column (but not a corner), then do the following: 
        else if (k==width & (j>=2 & j<=(height-1))) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Lcol[j-1,k] <- amounttomove # add amountomove UP
              remove_mass_Lcol[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){       
            new_mass_Lcol[j,k] <- 0 
            remove_mass_Lcol[j,k] <- 0
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Lcol[j+1,k] <- amounttomove # add amountomove DOWN
              remove_mass_Lcol[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_Lcol[j,k-1] <- amounttomove # add amountomove LEFT
              remove_mass_Lcol[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
        } # closes  if loop - through occupied last column 
        
        # if the cell is in the top left corner, then do the following:  
        else if (j==1 & k==1) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){        
            new_mass_TL[j,k] <- 0 
            remove_mass_TL[j,k] <- 0
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_TL[j,k+1] <- amounttomove # add amountomove RIGHT
              remove_mass_TL[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_TL[j+1,k] <- amounttomove # add amountomove DOWN
              remove_mass_TL[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){       
            new_mass_TL[j,k] <- 0 
            remove_mass_TL[j,k] <- 0
          }  
        } # closes  if loop - through occupied top left
        
        # if the cell is in the top right corner, then do the following:  
        else if (j==1 & k==width) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){       
            new_mass_TR[j,k] <- 0 
            remove_mass_TR[j,k] <- 0
          }
          if (direction_of_wind == "R"){       
            new_mass_TR[j,k] <- 0 
            remove_mass_TR[j,k] <- 0
          }
          if (direction_of_wind == "D"){
            if (x1[j+1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_TR[j+1,k] <- amounttomove # add amountomove DOWN
              remove_mass_TR[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_TR[j,k-1] <- amounttomove # add amountomove LEFT
              remove_mass_TR[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          
        } # closes  if loop - through occupied top right  
        
        # if the cell is in the bottom left corner, then do the following:  
        else if (j==height & k==1) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_BL[j-1,k] <- amounttomove # add amountomove UP
              remove_mass_BL[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){    
            if (x1[j,k+1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_BL[j,k+1] <- amounttomove # add amountomove RIGHT
              remove_mass_BL[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "D"){      
            new_mass_BL[j,k] <- 0 
            remove_mass_BL[j,k] <- 0
          }
          if (direction_of_wind == "L"){       
            new_mass_BL[j,k] <- 0 
            remove_mass_BL[j,k] <- 0
          }
        } # closes  if loop - through occupied bottom left 
        
        # if the cell is in the bottom right corner, then do the following:  
        else if (j==height & k==width) { 
          
          amounttomove <- x1[j,k] * proptomove # amount of cell biomass * proportion of occupied cell biomass that will move 
          
          if (direction_of_wind == "U"){  
            if (x1[j-1,k] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_BR[j-1,k] <- amounttomove # add amountomove UP
              remove_mass_BR[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
          if (direction_of_wind == "R"){       
            new_mass_BR[j,k] <- 0 
            remove_mass_BR[j,k] <- 0
          }
          if (direction_of_wind == "D"){      
            new_mass_BR[j,k] <- 0 
            remove_mass_BR[j,k] <- 0
          }
          if (direction_of_wind == "L"){
            if (x1[j,k-1] < full_thresh_wind) { # only move if the destination cell is not "too full" 
              new_mass_BR[j,k-1] <-  amounttomove # add amountomove LEFT
              remove_mass_BR[j,k] <- -amounttomove # subtract this value from the original spot 
            }
          }
        } # closes  if loop - through occupied bottom right  
        
      } # closes if loop - through occupied cells 
    } # closes for loop - through width/columns (k)    
  } # closes for loop - through height/rows (j)  
  
  x2 <- x1 + new_mass_C + remove_mass_C +
    new_mass_Trow + remove_mass_Trow +
    new_mass_Brow + remove_mass_Brow +
    new_mass_Fcol + remove_mass_Fcol +
    new_mass_Lcol + remove_mass_Lcol +
    new_mass_TL + remove_mass_TL +
    new_mass_TR + remove_mass_TR +
    new_mass_BL + remove_mass_BL +
    new_mass_BR + remove_mass_BR 
    
  return(x2)
}