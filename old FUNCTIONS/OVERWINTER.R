#######################################
# Overwintering function             #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$SPmatrix            #
# x2... LIST[[i+1]]$SPmatrix          #
# n... species                        #
#                                     #
# Created: MJ McCann 3/22/2013        #
#######################################

OVERWINTER <- function(x1,x2,n) {
  
  x2 <- x1 * speciesmatrix$overwinter[n] # this is completely arbitary !
    
  return(x2)
}

