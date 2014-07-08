#######################################
# Phosphorus uptake function          #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEP11()      #
# n... numbFPspecies                  #
#                                     #
# thisstep... LIST[[t]]               #  
# nextstep... LIST[[t+1]]             #
#                                     #
# Created: MJ McCann 3/22/2014        #
# Updated: 07/08/2014                 #
#######################################
# need to add something that stops this from going below 0 

UPTAKE_P21 <- function(thisstep,nextstep) {
  
  # new SAV biomass that grew on most recent timestep     
  newbiomassSAV <- sum(nextstep$SAV-thisstep$SAV) 
  
  # if the new biomasss is (-) (i.e., a loss) set it to 0 
  if (newbiomassSAV < 0) {newbiomassSAV <- 0}
  
  # uptake by SAV 
  PremovedSAV <- newbiomassSAV * specieslist$uptakeP[1]
  
  # new FP biomass that grew on most recent timestep 
  newbiomassFP <- rep(0,numbFPspecies)
  PremovedFP <- rep(0,numbFPspecies)
  for (i in 1:numbFPspecies){
    newbiomassFP[i] <- sum(nextstep$FP[[i]]-thisstep$FP[[i]])
    if (newbiomassFP[i] < 0) {newbiomassFP[i] <- 0}
    PremovedFP[i] <- newbiomassFP[i] * specieslist$uptakeP[i+1]
  }
  
  PremovedFP<-sum(PremovedFP)
  
  totalPremoved <- PremovedFP + PremovedSAV
  
  if ((thisstep$TOTALP*height*width - totalPremoved)/(height*width) >= 0){
    nextstep$TOTALP <- (thisstep$TOTALP*height*width - totalPremoved)/(height*width)
  } else {
    nextstep$TOTALP <- 0.00001
  }  
    
  return(nextstep$TOTALP)
}







