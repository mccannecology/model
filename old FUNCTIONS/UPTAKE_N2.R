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
  
  newbiomassSAV <- sum(x[[i+1]]$SAVmatrix-x[[i]]$SAVmatrix) # new SAV biomass that grew on most recent timestep     
  
  # if the new biomasss is (-) (i.e., a loss) set it to 0 
  if (newbiomassSAV < 0) {newbiomassSAV <- 0}
  
  if (n == 4) {
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    newbiomassFP3 <- sum(x[[i+1]]$FP3matrix-x[[i]]$FP3matrix) # new FP3 biomass that grew on most recent timestep     
    newbiomassFP4 <- sum(x[[i+1]]$FP4matrix-x[[i]]$FP4matrix) # new FP4 biomass that grew on most recent timestep     
    
    # if the new biomasss is (-) (i.e., a loss) set it to 0 
    if (newbiomassFP1 < 0) {newbiomassFP1 <- 0}
    if (newbiomassFP2 < 0) {newbiomassFP1 <- 0}
    if (newbiomassFP3 < 0) {newbiomassFP1 <- 0}
    if (newbiomassFP4 < 0) {newbiomassFP1 <- 0}
    
    totalNremoved <- newbiomassSAV * speciesmatrix$uptakeN[1] + 
                     newbiomassFP1 * speciesmatrix$uptakeN[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeN[3] +
                     newbiomassFP3 * speciesmatrix$uptakeN[4] +
                     newbiomassFP4 * speciesmatrix$uptakeN[5]
    
  }
  else if (n == 3) {
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    newbiomassFP3 <- sum(x[[i+1]]$FP3matrix-x[[i]]$FP3matrix) # new FP3 biomass that grew on most recent timestep     
    
    # if the new biomasss is (-) (i.e., a loss) set it to 0 
    if (newbiomassFP1 < 0) {newbiomassFP1 <- 0}
    if (newbiomassFP2 < 0) {newbiomassFP1 <- 0}
    if (newbiomassFP3 < 0) {newbiomassFP1 <- 0}
    
    totalNremoved <- newbiomassSAV * speciesmatrix$uptakeN[1] +   
                     newbiomassFP1 * speciesmatrix$uptakeN[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeN[3] +
                     newbiomassFP3 * speciesmatrix$uptakeN[4]
      
  }
  else if (n == 2){
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep     
    newbiomassFP2 <- sum(x[[i+1]]$FP2matrix-x[[i]]$FP2matrix) # new FP2 biomass that grew on most recent timestep     
    
    # if the new biomasss is (-) (i.e., a loss) set it to 0 
    if (newbiomassFP1 < 0) {newbiomassFP1 <- 0}
    if (newbiomassFP2 < 0) {newbiomassFP1 <- 0}
    
    totalNremoved <- newbiomassSAV * speciesmatrix$uptakeN[1] +   
                     newbiomassFP1 * speciesmatrix$uptakeN[2] + 
                     newbiomassFP2 * speciesmatrix$uptakeN[3] 
    
  } 
  else if (n == 1){
    newbiomassFP1 <- sum(x[[i+1]]$FP1matrix-x[[i]]$FP1matrix) # new FP1 biomass that grew on most recent timestep      
    
    # if the new biomasss is (-) (i.e., a loss) set it to 0 
    if (newbiomassFP1 < 0) {newbiomassFP1 <- 0}
    
    totalNremoved <- newbiomassSAV * speciesmatrix$uptakeN[1] + 
                     newbiomassFP1 * speciesmatrix$uptakeN[2]
    
  }
    
  if ((x[[i]]$TOTALN*height*width - totalNremoved)/(height*width) >= 0){
    temp <- (x[[i]]$TOTALN*height*width - totalNremoved)/(height*width)
  }
  else {
    temp <- 0.00001
  }
   
  x[[i+1]]$TOTALN <- temp

  return(x[[i+1]]$TOTALN)
}







