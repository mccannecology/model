#######################################
# Nitrogen release function           #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEP10()      #
#                                     #
# Created: MJ McCann 3/22/2014        #
# Updated: 4/2014                     #
#######################################

RELEASE_N20 <- function(x,i) {
  # newbiomass <- sum(x[[i+1]]$SPALLmatrix-x[[i]]$SPALLmatrix) + # new biomass that grew during the last year 
  #               sum(x[[i+1]]$SPALLmatrix-x[[i]]$SPALLmatrix)
  # reset nutrient to something lower than initial value 
  # incorporate the difference between LIST[[1]]$SPALLmatrix and LIST[[i]]$SPALLmatrix, where i is the 
  
  x[[i+1]]$TOTALN <- x[[1]]$TOTALN # this just resets TOTALN to its initial value - need subtract out biomass on i+1 - biomass on 1
  
  return(x[[i+1]]$TOTALN)
}





