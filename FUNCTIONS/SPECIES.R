##########################################
# Generates species matrix               #
# Used to loop through species           #
#                                        #  
# By: Michael J. McCann                  #
# Last Updated: 2/26/2014                #
##########################################

SPECIES <- function(x){  
  parameters <- read.csv("input02.csv")
  
  simulnumb <- x   
  
  # make the number of columns = #variables + 1 
  speciesmatrix <- matrix(data=0,nrow=parameters$numbspecies[simulnumb],ncol=5)
  speciesmatrix <- as.data.frame(speciesmatrix)
  colnames(speciesmatrix)<-c("species","maxrgr","agedead","initial","overwinter")
  
  # build a species matrix from the parameter values defined elsewhere
  speciesmatrix[1]<-seq(1,numbspecies,1) # a vector of species numbers 
  for (i in 1:numbspecies) {speciesmatrix[i,2] <- parameters[simulnumb,19+i]} # a vector of maxrgr - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,3] <- parameters[simulnumb,15+i]} # a vector of agedead - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,4] <- parameters[simulnumb,11+i]} # a vector of initial - length varies on number of species
  for (i in 1:numbspecies) {speciesmatrix[i,5] <- parameters[simulnumb,23+i]} # a vector of overwinter - length varies on number of species
    
  assign("speciesmatrix",speciesmatrix, envir = .GlobalEnv)
  
} 





