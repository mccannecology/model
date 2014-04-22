#######################################
# Phosphorus uptake function          #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEP10()      #
# n... numbFPspecies                  #
#                                     #
# Created: MJ McCann 3/22/2014        #
# Updated: 4/2014                     #
#######################################
# need to add something that stops this from going below 0 

UPTAKE_P2 <- function(x,i,n) {
  
  newbiomassSAV <- sum(x[[i+1]]$SAVmatrix-x[[i]]$SAVmatrix) # new FP1 biomass that grew on most recent timestep   
  
  if (n == 4) {
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    newbiomassFP3 <- sum(x[[i+1]]$FP3matrix-x[[i]]$FP3matrix) # new FP3 biomass that grew on most recent timestep     
    newbiomassFP4 <- sum(x[[i+1]]$FP4matrix-x[[i]]$FP4matrix) # new FP4 biomass that grew on most recent timestep     
    
    totalPremoved <- newbiomassSAV * speciesmatrix$uptakeP[1] +   
                     newbiomassFP1 * speciesmatrix$uptakeP[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeP[3] +
                     newbiomassFP3 * speciesmatrix$uptakeP[4] +
                     newbiomassFP3 * speciesmatrix$uptakeP[5]
    
  }
  else if (n == 3) {
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    newbiomassFP3 <- sum(x[[i+1]]$FP3matrix-x[[i]]$FP3matrix) # new FP3 biomass that grew on most recent timestep     
    
    totalPremoved <- newbiomassSAV * speciesmatrix$uptakeP[1] +   
                     newbiomassFP1 * speciesmatrix$uptakeP[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeP[3] +
                     newbiomassFP3 * speciesmatrix$uptakeP[4]
    
  }
  else if (n == 2){
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    
    totalPremoved <- newbiomassSAV * speciesmatrix$uptakeP[1] +   
                     newbiomassFP1 * speciesmatrix$uptakeP[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeP[3] 
    
  } 
  else if (n == 1){
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep      
  
    totalPremoved <- newbiomassSAV * speciesmatrix$uptakeP[1] +   
                     newbiomassFP1 * speciesmatrix$uptakeP[2] 
  
  }
  
  if ((x[[i]]$TOTALP*height*width - totalPremoved)/(height*width) >= 0){
    temp <- (x[[i]]$TOTALP*height*width - totalPremoved)/(height*width)
  }
  else {
    temp <- 0
  }
  
  x[[i+1]]$TOTALP <- temp
  
  return(x[[i+1]]$TOTALP)
  
  return(x[[i+1]]$TOTALP)
}


