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

UPTAKE_N29 <- function(thisstep,nextstep) {
  
  ###################################
  # Define function for uptake rate # 
  ################################### 
  uptake_rate <- function(TOTALN, rND){
    # corrected maximum uptake rate 
    uptake_max_cor <- cVNUptMax*cQ10Prod^(0.1*(temp-20))*((cNDmax-rND)/(cNDmax-cNDmin))
    
    # specific uptake rate 
    rate <- uptake_max_cor*(TOTALN/((uptake_max_cor/cAffNUpt)+TOTALN))
    rate
  }
  
  #########################
  # Common values for SAV #
  #########################
  # I used max. specified by Janse & van Puijenbroek 1997
  temp <- 20 # temperature 
  cVNUptMax <- 0.07 # max uptake rate 
  cNDmin <- 0.02  # min. ratio of nitrogen to plant dry mass
  cNDmax <- 0.05 # max. ratio of nitrogen to plant dry mass
  cQ10Prod <- 2 # factor by which growth rate increases due to a 10C increase in temp 
  cAffNUpt <- 0.02 # affinity for N [m3/gD/d]
  
  ############################
  # Values for newbiomassSAV #
  ############################
  rND_new <- 0.045 # current ratio of nitrogen to plant dry mass (assume it's the min. b/c only new veg. uptakes)
  
  ############################
  # Values for oldbiomassSAV #
  ############################
  rND_old <- 0.05 # current ratio of nitrogen to plant dry mass (assume it's the min. b/c only new veg. uptakes)
  
  ################
  # SAV - uptake # 
  ################
  # new SAV biomass that grew on most recent timestep     
  # sum() would compress this into a single #   
  oldbiomassSAV <- thisstep$SAV 
  newbiomassSAV <- nextstep$SAV-thisstep$SAV # this number will be (+) if biomass increased 

  # cells where biomass was lost - oldbiomass needs to decrease 
  oldbiomassSAV[newbiomassSAV<0] <- newbiomassSAV[newbiomassSAV<0] + oldbiomassSAV[newbiomassSAV<0] 
  newbiomassSAV[newbiomassSAV<0] <- 0 # and new biomass should be set to 0
  
  # uptake by SAV - allows for different uptake rates for new and old biomass 
  NremovedSAV <- (newbiomassSAV * uptake_rate(thisstep$TOTALN, rND=rND_new) * 1000) + 
                  (oldbiomassSAV * uptake_rate(thisstep$TOTALN, rND=rND_old) * 1000) 
  
  ##################################
  # Common values for newbiomassFP #
  ##################################
  # I used max. specified by Janse & van Puijenbroek 1997
  temp <- 20
  cVNUptMax <- 0.07 
  cNDmin <- 0.03
  cNDmax <- 0.1
  cQ10Prod <- 2.5
  cAffNUpt <- 0.02 
  
  ###########################
  # Values for newbiomassFP #
  ###########################
  rND_new <- 0.09

  ###########################
  # Values for oldbiomassFP #
  ###########################
  rND_old <- 0.1
  
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
    
    NremovedFP[[i]]  <- (newbiomassFP[[i]] * uptake_rate(thisstep$TOTALN,rND= rND_new) * 1000) + 
                        (oldbiomassFP[[i]] * uptake_rate(thisstep$TOTALN,rND= rND_old) * 1000) 
  }
  
  # Sum the removal by all FP species ("Reduce" combines elements of a list)  
  NremovedFP <- Reduce('+', NremovedFP)
  
  
  #######################
  # combined - SAV & FP # 
  ####################### 
  # combine removal by FP and SAV 
  totalNremoved <- NremovedFP + NremovedSAV
    
  # subtract off the total N that is removed
  nextstep$TOTALN <- thisstep$TOTALN-totalNremoved
  
  # convert any negative values to something really small
  nextstep$TOTALN[nextstep$TOTALN<0] <- 0.00001  
  
  nextstep$TOTALN[LAND==1] <- 0
  
  # return this value from the function 
  return(nextstep$TOTALN)
}

###########################
# try with some real data 
###########################