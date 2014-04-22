#######################################
# Nitrogen uptake function            #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEP10()      #
# n... species                        #
#                                     #
# Created: MJ McCann 3/22/2013        #
#######################################
# need to add something that stops this from going below 0 

UPTAKE_N <- function(x,i,n) {
  if (n == 4) {
    newbiomassSP1 <- sum(x[[i+1]]$SP1matrix-x[[i]]$SP1matrix) # new SP1 biomass that grew on most recent timestep     
    newbiomassSP2 <- sum(x[[i+1]]$SP2matrix-x[[i]]$SP2matrix) # new SP2 biomass that grew on most recent timestep     
    newbiomassSP3 <- sum(x[[i+1]]$SP3matrix-x[[i]]$SP3matrix) # new SP3 biomass that grew on most recent timestep     
    newbiomassSP4 <- sum(x[[i+1]]$SP4matrix-x[[i]]$SP4matrix) # new SP4 biomass that grew on most recent timestep     
    
    totalNremoved <- newbiomassSP1 * speciesmatrix$uptakeN[1] + 
                     newbiomassSP2 * speciesmatrix$uptakeN[2] +
                     newbiomassSP3 * speciesmatrix$uptakeN[3] +
                     newbiomassSP3 * speciesmatrix$uptakeN[4]
    
    x[[i+1]]$TOTALN <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  }
  else if (n == 3) {
    newbiomassSP1 <- sum(x[[i+1]]$SP1matrix-x[[i]]$SP1matrix) # new SP1 biomass that grew on most recent timestep     
    newbiomassSP2 <- sum(x[[i+1]]$SP2matrix-x[[i]]$SP2matrix) # new SP2 biomass that grew on most recent timestep     
    newbiomassSP3 <- sum(x[[i+1]]$SP3matrix-x[[i]]$SP3matrix) # new SP3 biomass that grew on most recent timestep     
    
    totalNremoved <- newbiomassSP1 * speciesmatrix$uptakeN[1] + 
                     newbiomassSP2 * speciesmatrix$uptakeN[2] +
                     newbiomassSP3 * speciesmatrix$uptakeN[3]
    
    x[[i+1]]$TOTALN <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  }
  else if (n == 2){
    newbiomassSP1 <- sum(x[[i+1]]$SP1matrix-x[[i]]$SP1matrix) # new SP1 biomass that grew on most recent timestep     
    newbiomassSP2 <- sum(x[[i+1]]$SP2matrix-x[[i]]$SP2matrix) # new SP2 biomass that grew on most recent timestep     
    
    totalNremoved <- newbiomassSP1 * speciesmatrix$uptakeN[1] + 
                     newbiomassSP2 * speciesmatrix$uptakeN[2]
    
    x[[i+1]]$TOTALN <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  } 
  else if (n == 1){
    newbiomassSP1 <- sum(x[[i+1]]$SP1matrix-x[[i]]$SP1matrix) # new SP1 biomass that grew on most recent timestep      
    
    totalNremoved <- newbiomassSP1 * speciesmatrix$uptakeN[1]
    
    x[[i+1]]$TOTALN <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  }
  return(x[[i+1]]$TOTALN)
}







