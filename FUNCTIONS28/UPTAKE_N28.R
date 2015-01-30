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

UPTAKE_N28 <- function(thisstep,nextstep) {
  
  #######
  # SAV # 
  #######
  
  # new SAV biomass that grew on most recent timestep     
  # sum() would compress this into a single #   
  oldbiomassSAV <- thisstep$SAV 
  newbiomassSAV <- nextstep$SAV-thisstep$SAV # this number will be (+) if biomass increased 

  # cells where biomass was lost - oldbiomass needs to decrease 
  oldbiomassSAV[newbiomassSAV<0] <- newbiomassSAV[newbiomassSAV<0] + oldbiomassSAV[newbiomassSAV<0] 
  newbiomassSAV[newbiomassSAV<0] <- 0 # and new biomass should be set to 0
  
  # uptake by SAV - allows for different uptake rates for new and old biomass 
  NremovedSAV <- (newbiomassSAV * specieslist$uptakeN[1]) + (oldbiomassSAV * specieslist$uptakeN[1])
  
  ######
  # FP # 
  ######
  
  # new FP biomass that grew on most recent timestep 
  newbiomassFP <- vector("list",numbFPspecies)
  oldbiomassFP <- vector("list",numbFPspecies)
  NremovedFP <- vector("list",numbFPspecies)
 
  # loop through all of the FP species 
  # figure out how much new biomass there is 
  # and how much N should be removed 
  for (i in 1:numbFPspecies){
    
    oldbiomassFP[[i]] <- thisstep$FP[[i]] 
    newbiomassFP[[i]] <- nextstep$FP[[i]]-thisstep$FP[[i]]
   
    # cells where biomass was lost - oldbiomass needs to decrease 
    oldbiomassFP[[i]][newbiomassFP[[i]]<0] <- newbiomassFP[[i]][newbiomassFP[[i]]<0] + oldbiomassFP[[i]][newbiomassFP[[i]]<0] 
    newbiomassFP[[i]][newbiomassFP[[i]]<0] <- 0 # and new biomass should be set to 0
    
    NremovedFP[[i]] <- (newbiomassFP[[i]] * specieslist$uptakeN[i+1]) + (oldbiomassFP[[i]] * specieslist$uptakeN[i+1])
  }
  
  # Sum the removal by all FP species ("Reduce" combines elements of a list)  
  NremovedFP <- Reduce('+', NremovedFP)
  
  
  ############
  # combined # 
  ############ 
  
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