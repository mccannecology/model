# x1... LIST[[i]]$SPALLmatrix
# x2... LIST[[i+1]]$SPALLmatrix
# x3... LIST[[i]]$TOTALP
# x4... LIST[[i+1]]$TOTALP

UPTAKE_P <- function(x1,x2,x3,x4) {
  newbiomass <- sum(x2-x1) # new biomass that grew on most recent     
  x4 <- x3 - newbiomass * 0.001 # this decrease is completely arbitrary !
  return(x4)
}

# need to add something that stops this from going below 0 