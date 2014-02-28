##########################################
# Generates species matrix               #
# Used to loop through species           #
#                                        #  
# By: Michael J. McCann                  #
# Last Updated: 2/26/2014                #
##########################################

SPECIES <- function(x){  
  simulnumb <- x 
  
  # make the number of columns = #variables + 1 
  speciesmatrix <- matrix(data=0,nrow=parameters[simulnumb,11],ncol=5)
  speciesmatrix <- as.data.frame(speciesmatrix)
  colnames(speciesmatrix)<-c("species","maxrgr","agedead","initial","overwinter")
  
  # build a species matrix from the parameter values defined elsewhere
  speciesmatrix[1]<-seq(1,numbspecies,1)    
  speciesmatrix[2]<-as.vector(parameters[simulnumb,20]:parameters[simulnumb,20+numbspecies-1])
  speciesmatrix[3]<-as.vector(parameters[simulnumb,16]:parameters[simulnumb,16+numbspecies-1])
  speciesmatrix[4]<-as.vector(parameters[simulnumb,12]:parameters[simulnumb,12+numbspecies-1])
  speciesmatrix[5]<-as.vector(parameters[simulnumb,24]:parameters[simulnumb,24+numbspecies-1])
  
  assign("speciesmatrix",speciesmatrix, envir = .GlobalEnv)
  
} 





