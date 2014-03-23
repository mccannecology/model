#######################################
# Phosphorus uptake function          #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEP10()      #
#                                     #
# Created: MJ McCann 3/22/2013        #
#######################################

UPTAKE_P <- function(x,i) {
  newbiomass <- sum(x[[i+1]]$SPALLmatrix-x[[i]]$SPALLmatrix) # new biomass that grew on most recent     
  x[[i+1]]$TOTALP <- x[[i]]$TOTALP - newbiomass * 0.001 # this decrease is completely arbitrary !
  return(x[[i+1]]$TOTALP)
}

# need to add something that stops this from going below 0 