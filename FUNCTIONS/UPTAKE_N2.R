#######################################
# Nitrogen uptake function            #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEP11()      #
# n... numbFPspecies                  #
#                                     #
# Created: MJ McCann 3/22/2014        #
# Updated: 4/2014                     #
#######################################
# need to add something that stops this from going below 0 

UPTAKE_N2 <- function(x,i,n) {
  
  newbiomassSAV <- sum(x[[i+1]]$SAVmatrix-x[[i]]$SAVmatrix) # new FP1 biomass that grew on most recent timestep     
  
  if (n == 4) {
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    newbiomassFP3 <- sum(x[[i+1]]$FP3matrix-x[[i]]$FP3matrix) # new FP3 biomass that grew on most recent timestep     
    newbiomassFP4 <- sum(x[[i+1]]$FP4matrix-x[[i]]$FP4matrix) # new FP4 biomass that grew on most recent timestep     
    
    totalNremoved <- newbiomassSAV * speciesmatrix$uptakeN[1] + 
                     newbiomassFP1 * speciesmatrix$uptakeN[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeN[3] +
                     newbiomassFP3 * speciesmatrix$uptakeN[4] +
                     newbiomassFP3 * speciesmatrix$uptakeN[5]
    
    x[[i+1]]$TOTALN <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  }
  else if (n == 3) {
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    newbiomassFP3 <- sum(x[[i+1]]$FP3matrix-x[[i]]$FP3matrix) # new FP3 biomass that grew on most recent timestep     
    
    totalNremoved <- newbiomassSAV * speciesmatrix$uptakeN[1] +   
                     newbiomassFP1 * speciesmatrix$uptakeN[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeN[3] +
                     newbiomassFP3 * speciesmatrix$uptakeN[4]
    
    x[[i+1]]$TOTALN <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  }
  else if (n == 2){
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    
    totalNremoved <- newbiomassSAV * speciesmatrix$uptakeN[1] +   
                     newbiomassFP1 * speciesmatrix$uptakeN[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeN[3] 
    
    x[[i+1]]$TOTALN <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  } 
  else if (n == 1){
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep      
    
    totalNremoved <- newbiomassSAV * speciesmatrix$uptakeN[1] + 
                     newbiomassFP1 * speciesmatrix$uptakeN[2]
    
    x[[i+1]]$TOTALN <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  }
  return(x[[i+1]]$TOTALN)
}







