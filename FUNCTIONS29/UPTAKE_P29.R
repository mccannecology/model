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

UPTAKE_P29 <- function(thisstep,nextstep) {
  
  ###################################
  # Define function for uptake rate # 
  ################################### 
  uptake_rate <- function(TOTALP, rNP){
    # corrected maximum uptake rate 
    uptake_max_cor <- cVNUptMax*cQ10Prod^(0.1*(temp-20))*((cPDmax-rNP)/(cPDmax-cPDmin))
    
    # specific uptake rate 
    rate <- uptake_max_cor*(TOTALP/((uptake_max_cor/cAffNUpt)+TOTALP))
    rate
  }
  
  ############################
  # Values for newbiomassSAV #
  ############################
  # I used the median between the min. and max. specified by Janse & van Puijenbroek 1997
  temp <- 20 # temperature 
  cVNUptMax <- 0.0385 # max uptake rate 
  rNP <- 0.0125 # current ratio of nitrogen to plant dry mass (assume it's the min. b/c only new veg. uptakes)
  cPDmin <- 0.0125  # min. ratio of nitrogen to plant dry mass
  cPDmax <- 0.04 # max. ratio of nitrogen to plant dry mass
  cQ10Prod <- 1.75 # factor by which growth rate increases due to a 10C increase in temp 
  cAffNUpt <- 0.011 # affinity for N [m3/gD/d]
  
  ############################
  # Values for oldbiomassSAV #
  ############################
  # I used the median between the min. and max. specified by Janse & van Puijenbroek 1997
  temp <- 20 # temperature 
  cVNUptMax <- 0.0385 # max uptake rate 
  rNP <- 0.0375 # current ratio of nitrogen to plant dry mass (assume it's the min. b/c only new veg. uptakes)
  cPDmin <- 0.0125  # min. ratio of nitrogen to plant dry mass
  cPDmax <- 0.04 # max. ratio of nitrogen to plant dry mass
  cQ10Prod <- 1.75 # factor by which growth rate increases due to a 10C increase in temp 
  cAffNUpt <- 0.011 # affinity for N [m3/gD/d]
  
  ################
  # SAV - uptake # 
  ################
  # new SAV biomass that grew on most recent timestep     
  # sum() would compress this into a single #   
  oldbiomassSAV <- thisstep$SAV 
  newbiomassSAV <- nextstep$SAV-thisstep$SAV # this number will be (+) if biomass increased 

  # cells where biomass was lost - oldbiomass needs to decrease 
  oldbiomassSAV[newbiomassSAV<0] <- newbiomassSAV[newbiomassSAV<0] + oldbiomassSAV[newbiomassSAV<0] 
  oldbiomassSAV[newbiomassSAV<0] <- 0 # and new biomass should be set to 0
  
  # uptake by SAV - allows for different uptake rates for new and old biomass 
  NremovedSAV <- (newbiomassSAV * uptake_rate(thisstep$TOTALP,rNP=0.0125)) + (oldbiomassSAV * uptake_rate(thisstep$TOTALP,rNP=0.0375)) 
  
  ###########################
  # Values for newbiomassFP #
  ###########################
  # I used the median between the min. and max. specified by Janse & van Puijenbroek 1997
  temp <- 20
  cVNUptMax <- 0.0385 
  rNP <- 0.02
  cPDmin <- 0.02
  cPDmax <- 0.75
  cQ10Prod <- 2.25
  cAffNUpt <- 0.011 
  
  ###########################
  # Values for oldbiomassFP #
  ###########################
  # I used the median between the min. and max. specified by Janse & van Puijenbroek 1997
  temp <- 20
  cVNUptMax <- 0.0385 
  rNP <- 0.72
  cPDmin <- 0.02
  cPDmax <- 0.75
  cQ10Prod <- 2.25
  cAffNUpt <- 0.011 
  
  ###############
  # FP - uptake # 
  ###############
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
    
    NremovedFP[[i]]  <- (newbiomassFP[[i]] * uptake_rate(thisstep$TOTALP,rNP=0.02)) + (oldbiomassFP[[i]] * uptake_rate(thisstep$TOTALP,rNP=0.72)) 
  }
  
  # Sum the removal by all FP species ("Reduce" combines elements of a list)  
  NremovedFP <- Reduce('+', NremovedFP)
  
  
  #######################
  # combined - SAV & FP # 
  ####################### 
  # combine removal by FP and SAV 
  TOTALPremoved <- NremovedFP + NremovedSAV
    
  # subtract off the total N that is removed
  nextstep$TOTALP <- thisstep$TOTALP-TOTALPremoved
  
  # convert any negative values to something really small
  nextstep$TOTALP[nextstep$TOTALP<0] <- 0.00001  
  
  # return this value from the function 
  return(nextstep$TOTALP)
}

###########################
# try with some real data 
###########################