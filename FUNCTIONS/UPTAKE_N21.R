#######################################
# Nitrogen uptake function            #
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

UPTAKE_N21 <- function(thisstep,nextstep) {
  
  # new SAV biomass that grew on most recent timestep     
  newbiomassSAV <- sum(nextstep$SAV-thisstep$SAV) 
  
  # if the new biomasss is (-) (i.e., a loss) set it to 0 
  if (newbiomassSAV < 0) {newbiomassSAV <- 0}
  
  # uptake by SAV 
  NremovedSAV <- newbiomassSAV * specieslist$uptakeN[1]
  
  # new FP biomass that grew on most recent timestep 
  newbiomassFP <- rep(0,numbFPspecies)
  NremovedFP <- rep(0,numbFPspecies)
  for (i in 1:numbFPspecies){
    newbiomassFP[i] <- sum(nextstep$FP[[i]]-thisstep$FP[[i]])
    if (newbiomassFP[i] < 0) {newbiomassFP[i] <- 0}
    NremovedFP[i] <- newbiomassFP[i] * specieslist$uptakeN[i+1]
  }
  
  NremovedFP<-sum(NremovedFP)
  
  totalNremoved <- NremovedFP + NremovedSAV
  
  if ((thisstep$TOTALN*height*width - totalNremoved)/(height*width) >= 0){
    nextstep$TOTALN <- (thisstep$TOTALN*height*width - totalNremoved)/(height*width)
  } else {
    nextstep$TOTALN <- 0.00001
  }  
    
  return(nextstep$TOTALN)
}







