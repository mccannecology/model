#######################################
# Nitrogen uptake function            #
# Compatible w/ new LIST structure    #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEP10()      #
#                                     #
# Created: MJ McCann 3/22/2013        #
#######################################

UPTAKE_N <- function(x,i) {
  newbiomass <- sum(x[[i+1]]$SPALLmatrix-x[[i]]$SPALLmatrix) # new biomass that grew on most recent     
  x[[i+1]]$TOTALN <- x[[i]]$TOTALN - newbiomass * 0.001 # this decrease is completely arbitrary !
  return(x[[i+1]]$TOTALN)
}

# need to add something that stops this from going below 0 

# species should differ in their uptake rates 
# add an object for each species' newbiomass e.g., newbiomassSP1 



