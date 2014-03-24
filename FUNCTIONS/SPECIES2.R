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
  speciesmatrix <- matrix(data=0,nrow=parameters$numbspecies[simulnumb],ncol=9)
  
  speciesmatrix <- as.data.frame(speciesmatrix)
  
  colnames(speciesmatrix)<-c("species","maxrgr","initial","overwinter","halfsatB","halfsatN","halfsatP","uptakeN","uptakeP")
  
  # build a species matrix from the parameter values defined elsewhere
  speciesmatrix[1]<-seq(1,numbspecies,1) # a vector of species numbers 
  
  # REMEMBER: parameters[simulnumb,9+i] for maxrgr means it starts @ colmn 10 
  for (i in 1:numbspecies) {speciesmatrix[i,2] <- parameters[simulnumb,12+i]} # a vector of maxrgr - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,3] <- parameters[simulnumb,8+i]} # a vector of initial - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,4] <- parameters[simulnumb,16+i]} # a vector of overwinter - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,5] <- parameters[simulnumb,20+i]} # a vector of halfsatB - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,6] <- parameters[simulnumb,24+i]} # a vector of halfsatN - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,7] <- parameters[simulnumb,28+i]} # a vector of halfsatP - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,8] <- parameters[simulnumb,32+i]} # a vector of uptakeN - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,9] <- parameters[simulnumb,36+i]} # a vector of uptakeP - length varies on number of species
  
  assign("speciesmatrix",speciesmatrix, envir = .GlobalEnv)
  
} 





