##########################################
# Generates species matrix               #
# Used to loop through species           #
# Compatible with new LIST structure     #
#                                        #  
# By: Michael J. McCann                  #
# Major revision: 3/24/2014              #
##########################################

SPECIES2 <- function(x){  
  
  # make the number of columns = #variables + 1 
  speciesmatrix <- matrix(data=0,nrow=parameters$numbspecies[simulnumb],ncol=10)
  
  speciesmatrix <- as.data.frame(speciesmatrix)
  
  colnames(speciesmatrix)<-c("species","maxrgr","initial_cells","initial_total_biomass","overwinter","halfsatB","halfsatN","halfsatP","uptakeN","uptakeP")
  
  # build a species matrix from the parameter values defined elsewhere
  speciesmatrix[1]<-seq(1,numbspecies,1) # a vector of species numbers 
  
  # REMEMBER: parameters[simulnumb,9+i] for maxrgr means it starts @ colmn 10 
  for (i in 1:numbspecies) {speciesmatrix[i,2] <- parameters[simulnumb,19+i]} # a vector of maxrgr - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,3] <- parameters[simulnumb,11+i]} # a vector of initial_cells - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,4] <- parameters[simulnumb,15+i]} # a vector of initial_total_biomass - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,5] <- parameters[simulnumb,23+i]} # a vector of overwinter - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,6] <- parameters[simulnumb,27+i]} # a vector of halfsatB - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,7] <- parameters[simulnumb,31+i]} # a vector of halfsatN - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,8] <- parameters[simulnumb,35+i]} # a vector of halfsatP - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,9] <- parameters[simulnumb,39+i]} # a vector of uptakeN - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,10] <- parameters[simulnumb,43+i]} # a vector of uptakeP - length varies on number of species
  
  assign("speciesmatrix",speciesmatrix, envir = .GlobalEnv)
  
} 





