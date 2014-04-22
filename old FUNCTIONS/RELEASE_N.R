#######################################
# Nitrogen release function           #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEP10()      #
#                                     #
# Created: MJ McCann 3/22/2013        #
#######################################

RELEASE_N <- function(x,i) {
  newbiomass <- sum(x[[i+1]]$SPALLmatrix-x[[i]]$SPALLmatrix) # new biomass that grew on most recent 
  
  x[[i+1]]$TOTALN <- x[[1]]$TOTALN # this just resets TOTALN to its initial value - need subtract out biomass on i+1 - biomass on 1
  
  return(x[[i+1]]$TOTALN)
}





