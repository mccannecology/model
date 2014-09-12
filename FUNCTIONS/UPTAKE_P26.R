#######################################
# Phosphorus uptake function          #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# INPUTS:                             #
# x... LIST                           #
# i... from for loop in STEPxx()      #
# n... numbFPspecies                  #
#                                     #
# thisstep... LIST[[t]]               #  
# nextstep... LIST[[t+1]]             #
#                                     #
# Created: MJ McCann 3/22/2014        #
# Updated: Sept. 2014 - spatial nutr. #
#######################################
# need to add something that stops this from going below 0 

UPTAKE_P26 <- function(thisstep,nextstep) {
  
  # new SAV biomass that grew on most recent timestep     
  # sum() would compress this into a single #   
  newbiomassSAV <- nextstep$SAV-thisstep$SAV
  
  # if the new biomasss is (-) (i.e., a loss) set it to 0 
  newbiomassSAV[newbiomassSAV<0] <- 0
    
  # uptake by SAV 
  PremovedSAV <- newbiomassSAV * specieslist$uptakeP[1]
  
  # new FP biomass that grew on most recent timestep 
  newbiomassFP <- vector("list",numbFPspecies)
    
  PremovedFP <- vector("list",numbFPspecies)
 
  # loop through all of the FP species 
  # figure out how much new biomass there is 
  # and how much N should be removed 
  for (i in 1:numbFPspecies){
    
    newbiomassFP[[i]] <- nextstep$FP[[i]]-thisstep$FP[[i]]
   
    newbiomassFP[[i]][newbiomassFP[[i]] < 0] <- 0
    
    PremovedFP[[i]] <- newbiomassFP[[i]] * specieslist$uptakeP[i+1]
  }
  
  # add removal by all FP species ("Reduce" combines elements of a list)
  PremovedFP <- Reduce('+', PremovedFP)
  
  # combine removal by FP and SAV 
  totalPremoved <- PremovedFP + PremovedSAV
  
  # subtract off the total N that is removed
  nextstep$TOTALP <- thisstep$TOTALP-totalPremoved
  
  # convert any negative values to something really small
  nextstep$TOTALP[nextstep$TOTALP<0] <- 0.00001  
  
  return(nextstep$TOTALP)
}

###########################
# try with some real data 
###########################

# newbiomassSAV <- LIST[[2]]$SAV-LIST[[1]]$SAV

# if the new biomasss is (-) (i.e., a loss) set it to 0 
# newbiomassSAV[newbiomassSAV<0] <- 0

# uptake by SAV 
# PremovedSAV <- newbiomassSAV * specieslist$uptakeP[1]

# new FP biomass that grew on most recent timestep 
# newbiomassFP <- vector("list",numbFPspecies)

# PremovedFP <- vector("list",numbFPspecies)

# for (i in 1:numbFPspecies){
  
  # newbiomassFP[[i]] <- LIST[[2]]$FP[[i]]-LIST[[1]]$FP[[i]]
  
  # newbiomassFP[[i]][newbiomassFP[[i]] < 0] <- 0
  
  # PremovedFP[[i]] <- newbiomassFP[[i]] * specieslist$uptakeP[i+1]
# }

# combine the Premoved by all FP species - Reduce adds elements of a list 
# PremovedFP <- Reduce('+', PremovedFP)

# totalPremoved <- PremovedFP + PremovedSAV

# LIST[[2]]$TOTALP[LIST[[1]]$TOTALP - totalPremoved >= 0] <- LIST[[1]]$TOTALP - totalPremoved
# LIST[[2]]$TOTALP[LIST[[1]]$TOTALP - totalPremoved < 0] <- rep(0.00001,length(LIST[[2]]$TOTALP[LIST[[1]]$TOTALP - totalPremoved < 0]))



