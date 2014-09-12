#######################################
# Nitrogen uptake function            #
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

UPTAKE_N26 <- function(thisstep,nextstep) {
  
  # new SAV biomass that grew on most recent timestep     
  # sum() would compress this into a single #   
  newbiomassSAV <- nextstep$SAV-thisstep$SAV
  
  # if the new biomasss is (-) (i.e., a loss) set it to 0 
  newbiomassSAV[newbiomassSAV<0] <- 0
    
  # uptake by SAV 
  NremovedSAV <- newbiomassSAV * specieslist$uptakeN[1]
  
  # new FP biomass that grew on most recent timestep 
  newbiomassFP <- vector("list",numbFPspecies)
    
  NremovedFP <- vector("list",numbFPspecies)
 
  # loop through all of the FP species 
  # figure out how much new biomass there is 
  # and how much N should be removed 
  for (i in 1:numbFPspecies){
    
    newbiomassFP[[i]] <- nextstep$FP[[i]]-thisstep$FP[[i]]
   
    newbiomassFP[[i]][newbiomassFP[[i]] < 0] <- 0
    
    NremovedFP[[i]] <- newbiomassFP[[i]] * specieslist$uptakeN[i+1]
  }
  
  # add removal by all FP species ("Reduce" combines elements of a list)
  NremovedFP <- Reduce('+', NremovedFP)
  
  # combine removal by FP and SAV 
  totalNremoved <- NremovedFP + NremovedSAV
    
  # subtract off the total N that is removed
  nextstep$TOTALN <- thisstep$TOTALN-totalNremoved
  
  # convert any negative values to something really small
  nextstep$TOTALN[nextstep$TOTALN<0] <- 0.00001  
  
  # return this value from the function 
  return(nextstep$TOTALN)
}

###########################
# try with some real data 
###########################
# new SAV biomass that grew on most recent timestep     
# sum() would compress this into a single #   
# newbiomassSAV <- LIST[[2]]$SAV-LIST[[1]]$SAV

# if the new biomasss is (-) (i.e., a loss) set it to 0 
# newbiomassSAV[newbiomassSAV<0] <- 0

# uptake by SAV 
# NremovedSAV <- newbiomassSAV * specieslist$uptakeN[1]

# new FP biomass that grew on most recent timestep 
# newbiomassFP <- vector("list",numbFPspecies)

# NremovedFP <- vector("list",numbFPspecies)

# loop through all of the FP species 
# figure out how much new biomass there is 
# and how much N should be removed 
# for (i in 1:numbFPspecies){
  
# newbiomassFP[[i]] <- LIST[[2]]$FP[[i]]-LIST[[1]]$FP[[i]]
  
# newbiomassFP[[i]][newbiomassFP[[i]] < 0] <- 0
  
# NremovedFP[[i]] <- newbiomassFP[[i]] * specieslist$uptakeN[i+1]
# }

# add removal by all FP species ("Reduce" combines elements of a list)
# NremovedFP <- Reduce('+', NremovedFP)

# combine removal by FP and SAV 
# totalNremoved <- NremovedFP + NremovedSAV

# if the amount of TOTAL N left over is greater than 0, remove it 
# LIST[[2]]$TOTALN[LIST[[1]]$TOTALN - totalNremoved >= 0] <- LIST[[1]]$TOTALN - totalNremoved
# if the amount of TOTAL N left over is less than 0, make it a really small amount  
# LIST[[2]]$TOTALN[LIST[[1]]$TOTALN - totalNremoved < 0] <- rep(0.00001,length(LIST[[2]]$TOTALN[LIST[[1]]$TOTALN - totalNremoved < 0]))


