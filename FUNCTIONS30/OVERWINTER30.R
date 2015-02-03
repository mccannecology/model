#######################################
# Overwintering function              #
# Floating plants only                #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$SPmatrix            #
# i... numbFPspecies                  #
#                                     #
# Created: MJ McCann 3/22/2014        #
# Updated: 7/8/2014                   #
#######################################

OVERWINTER30 <- function(x1,i) {
  
  x1[[i]] <- x1[[i]] * specieslist$overwinter[i+1]
    
  return(x1[[i]])
}

