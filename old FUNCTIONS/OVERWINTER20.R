#######################################
# Overwintering function              #
# Floating plants only                #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$SPmatrix            #
# i... numbFPspecies                  #
#                                     #
# Created: MJ McCann 3/22/2013        #
#######################################

OVERWINTER20 <- function(x1,i) {
  
  x1[[i]] <- x1[[i]] * speciesmatrix$overwinter[i+1] # parameter value is completely arbitary !
    
  return(x1[[i]])
}

