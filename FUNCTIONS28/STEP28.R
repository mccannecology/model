#################################
# Main function                 #
# calls GROW(), MOVE(),         #
# ...WIND(), UPTAKE(), etc.     # 
#                               #
# INPUTS:                       #
# x... LIST[[t]]                #
# t... timestep                 #
#                               #
# thisstep... LIST[[t]]         #
# nextstep... LIST[[t+1]]       #
#                               #
# OUTPUT:                       #
# nextstep                      #
#                               #
# updated: 07/08/2014 MJM       #
#################################

#########################################
# load workspace for de-bugging 
# LIST has an initial time step only 
# short (3 yrs, 50 days each)
# one FP species
# load("testworkspace-1FPspecies.Rdata") 
# four FP species
# load("testworkspace-4FPspecies.Rdata") 
#########################################

STEP28 <- function(x,t){
  
  thisstep <- x 
  nextstep <- x
  
  # define a new operator - opposite of %in% - i.e., not in a subset
  "%ni%" <- Negate("%in%") 
  
  # check if it's NOT overwintering timestep, then do the following 
  if (t %ni% winters){
  
    ############
    # Grow SAV #
    ############
    nextstep$SAV <- GROW_SAV28(thisstep$SAV,
                               thisstep$FPtotal,
                               thisstep$TOTALP,
                               thisstep$TOTALN,
                               LAND) 
    
    ###########
    # Grow FP #
    ###########  
    for (j in 1:numbFPspecies) {
      nextstep$FP[[j]]<-GROW_FP28(thisstep$FP,
                                  thisstep$FPtotal,
                                  j,
                                  thisstep$TOTALP,
                                  thisstep$TOTALN,
                                  LAND) 
    }
    
    ####################
    # UPTAKE NUTRIENTS # 
    ####################
    # Check if it is a mixing day (according to mix_freq) 
    if (t %in% mix_days){
      
      # TOTAL P # 
      # uptake 
      nextstep$TOTALP <- UPTAKE_P28(thisstep,nextstep)
      
      # mix 
      nextstep$TOTALP[LAND==0] <- sum(nextstep$TOTALP)/area
                 
      
      # TOTAL N # 
      # uptake 
      nextstep$TOTALN <- UPTAKE_N28(thisstep,nextstep)
      
      # mix 
      nextstep$TOTALN[LAND==0] <- sum(nextstep$TOTALN)/area

    }
    
    # Time steps when it is not a mixing day (according to mix_freq) 
    else{
      nextstep$TOTALN <- UPTAKE_N28(thisstep,nextstep)
      nextstep$TOTALP <- UPTAKE_P28(thisstep,nextstep)
    }
    
    ############
    # Move SAV #
    ############
    # raster version of MOVE() 
    nextstep$SAV <- MOVE28(nextstep$SAV,neigh_thresh_SAV,focal_thresh_SAV,amnt_colonize_SAV)
    
    ###########
    # Move FP #
    ###########
    # raster version of MOVE() 
    nextstep$FP <- lapply(nextstep$FP,function(x){
      MOVE28(x,neigh_thresh_FP,focal_thresh_FP,amnt_colonize_FP)
    })
    
    ###########
    # Wind FP #
    ###########
    # some weird things happen - some cells move small amounts many spaces   
    # Sample direction and strength of wind 
    direction_sample <- c(rep("U",prob_up*100),
                          rep("D",prob_down*100),
                          rep("L",prob_left*100),
                          rep("R",prob_right*100)) # make a vector of wind directions to sample from
    direction_of_wind <- sample(direction_sample,1) # sample from it 
    proptomove <- rbeta(1,wind_shape1,wind_shape2) # assign an amount to move on the given time-step based on a beta distribution
        
    if (proptomove < 0) {proptomove <- 0} # make any negative values 0 
    if (proptomove > 1) {proptomove <- 1} # cannot move >100% of a cell  
    
    nextstep$FP <- lapply(nextstep$FP,function(x){
      WIND28(x,proptomove,direction_of_wind,full_thresh_wind=800) 
    })
  
    ##########
    # Sum FP # 
    ##########
    # combine multiple FP species into "FPtotal" 
    nextstep$FPtotal <- Reduce('+', nextstep$FP)
    
  } # close if statement for non-overwintering
  
  ##############
  # OVERWINTER #
  ##############
  else{
    ###########
    # DIE-OFF #
    ###########
    nextstep$SAV <- thisstep$SAV * specieslist$overwinter[1]
      
    for (i in 1:numbFPspecies) {
      nextstep$FP[[i]]<-OVERWINTER28(thisstep$FP,i) 
    }
    
    nextstep$FPtotal <- Reduce('+', nextstep$FP)
    
    #############
    # RELEASE N #
    #############
    nextstep$TOTALN <- LIST[[1]]$TOTALN
    
    #############
    # RELEASE P #
    #############
    nextstep$TOTALP <- LIST[[1]]$TOTALP
    
  } # close else statement for overwintering
  return(nextstep)
}
