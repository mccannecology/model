##########################################
# Generating species                     #
# STILL IN DEVELOPMENT                   #
#                                        #  
# By: Michael J. McCann                  #
# Last Updated: 2/26/2014                #
##########################################

SPECIES <- function(numbspecies) {
  for ()
} 

# make the number of columns = #variables + 1 
speciesmatrix <- matrix(data=0,nrow=numbspecies,ncol=5)
speciesmatrix <- as.data.frame(speciesmatrix)
colnames(speciesmatrix)<-c("species","maxrgr","agedead","initial","overwinter")


# build a species matrix if you already defined the parameter values elsewhere
speciesmatrix[1]<-c(1,2,3,4)
speciesmatrix[2]<-c(maxrgr01,maxrgr02,maxrgr03,maxrgr04)
speciesmatrix[3]<-c(agedead01,agedead02,agedead03,agedead04)
speciesmatrix[4]<-c(initial01,initial02,initial03,initial04)
speciesmatrix[5]<-c(overwinter01,overwinter02,overwinter03,overwinter04)

numbspecies <- nrow(speciesmatrix)