# x... LIST
# i... i from the timestep for loop in STEP10()

UPTAKE_P <- function(x,i) {
  newbiomass <- sum(x[[i+1]]$SPALLmatrix-x[[i]]$SPALLmatrix) # new biomass that grew on most recent     
  x[[i+1]]$TOTALP <- x[[i]]$TOTALP - newbiomass * 0.001 # this decrease is completely arbitrary !
  return(x[[i+1]]$TOTALP)
}

# need to add something that stops this from going below 0 