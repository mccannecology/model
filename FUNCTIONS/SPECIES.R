##########################################
# Generates species matrix               #
# Used to loop through species           #
# STILL IN DEVELOPMENT                   #
#                                        #  
# By: Michael J. McCann                  #
# Last Updated: 2/26/2014                #
##########################################

SPECIES <- function(){
  # make the number of columns = #variables + 1 
  speciesmatrix <- matrix(data=0,nrow=numbspecies,ncol=5)
  speciesmatrix <- as.data.frame(speciesmatrix)
  colnames(speciesmatrix)<-c("species","maxrgr","agedead","initial","overwinter")
  
  # build a species matrix from the parameter values defined elsewhere
  speciesmatrix[1]<-seq(1,numbspecies,1)    
  speciesmatrix[2]<-as.vector(input[simulation,20]:input[simulation,20+numbspecies-1])
  speciesmatrix[3]<-as.vector(input[simulation,16]:input[simulation,16+numbspecies-1])
  speciesmatrix[4]<-as.vector(input[simulation,12]:input[simulation,12+numbspecies-1])
  speciesmatrix[5]<-as.vector(input[simulation,24]:input[simulation,24+numbspecies-1])
} 





