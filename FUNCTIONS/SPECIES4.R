##########################################
# Generates species matrix               #
# Used to loop through species           #
# Compatible with new LIST structure     #
# Modified to work w/ SAV component      # 
#                                        #
# Created by: MJM - March 21, 2014       #
# Updated: April 2014                    # 
##########################################

SPECIES4 <- function(x){  
  
  # make the number of columns = #variables + 1 
  speciesmatrix <- matrix(data=0,nrow=parameters$numbFPspecies[simulnumb]+1,ncol=10)
  
  speciesmatrix <- as.data.frame(speciesmatrix)
  
  colnames(speciesmatrix)<-c("species","maxrgr","initial_cells","initial_total_biomass","overwinter","halfsatB","halfsatN","halfsatP","uptakeN","uptakeP")
  
  # build a species matrix from the parameter values defined elsewhere
  speciesmatrix[1]<-c("FP",seq(1,numbFPspecies,1)) # a vector of species numbers 
  
  # REMEMBER: parameters[simulnumb,9+i] for maxrgr means it starts @ colmn 10 
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,2] <- parameters[simulnumb,25+i]} # a vector of maxrgr - length varies on number of species
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,3] <- parameters[simulnumb,15+i]} # a vector of initial_cells 
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,4] <- parameters[simulnumb,20+i]} # a vector of initial_total_biomass 
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,5] <- parameters[simulnumb,30+i]} # a vector of overwinter 
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,6] <- parameters[simulnumb,25+i]} # a vector of halfsatB 
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,7] <- parameters[simulnumb,40+i]} # a vector of halfsatN 
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,8] <- parameters[simulnumb,45+i]} # a vector of halfsatP 
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,9] <- parameters[simulnumb,50+i]} # a vector of uptakeN 
  for (i in 1:(numbFPspecies+1)) {speciesmatrix[i,10] <- parameters[simulnumb,55+i]} # a vector of uptakeP 
  
  return(speciesmatrix)
  
} 





