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
# updated: 7/6/2014 MJM         #
#################################

######################################
# load workspace for de-bugging 
# LIST has an initial time step only 
# short (3 yrs, 50 days each)
# load("testworkspace.Rdata")
######################################

STEP20<-function(x,t){
  
  thisstep <- x 
  nextstep <- x
  
  # define a new operator - opposite of %in% - i.e., not in a subset
  "%ni%" <- Negate("%in%") 
  
  # check if it's NOT overwintering timestep, then do the following 
  if (t %ni% winters){
  
    ############
    # Grow SAV #
    ############
    nextstep$SAV <- GROW_SAV20(thisstep$SAV,thisstep$FPtotal,thisstep$TOTALP,thisstep$TOTALN) 
    
    ###########
    # Grow FP #
    ###########  
    for (j in 1:numbFPspecies) {
      nextstep$FP[[j]]<-GROW_FP20(thisstep$FP,thisstep$FPtotal,j,thisstep$TOTALP,thisstep$TOTALN) 
    }
    
    ############
    # Move SAV #
    ############
    # raster version of MOVE() 
    nextstep$SAV <- MOVE_with_raster(nextstep$SAV,neigh_thresh_SAV,focal_thresh_SAV,amnt_colonize_SAV)
    
    # old version of MOVE()
    #nextstep$SAV <- MOVE_SAV20(nextstep$SAV,full_threshold=150)
    
    ###########
    # Move FP #
    ###########
    # raster version of MOVE() 
    nextstep$FP <- lapply(nextstep$FP,function(x){
      MOVE_with_raster(x,neigh_thresh_FP,focal_thresh_FP,amnt_colonize_FP)
    })
    
    # old version of MOVE()
    #nextstep$FP <- lapply(nextstep$FP,function(x){
    #  MOVE_FP20(x)
    #})
      
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
    proptomove <- rnorm(1,wind_avg,wind_std) # assign an amount to move on the given time-step based on the average & SD of windiness 
    
    if (proptomove < 0) {proptomove <- 0} # make any negative values 0 
    if (proptomove > 1) {proptomove <- 1} # cannot move >100% of a cell  
    
    nextstep$FP <- lapply(nextstep$FP,function(x){
      WIND20(x,proptomove,direction_of_wind,full_threshold=800) 
    })
  
    ##########
    # Sum FP #
    ##########
    nextstep$FPtotal <- Reduce('+', nextstep$FP)
    
    ############
    # Uptake N #
    ############
    nextstep$TOTALN <- UPTAKE_N20(thisstep,nextstep)
    
    ############
    # Uptake P #
    ############ 
    nextstep$TOTALP <- UPTAKE_P20(thisstep,nextstep)
  } # close if statement for non-overwintering
  
  ##############
  # OVERWINTER #
  ##############
  else{
    ###########
    # DIE-OFF #
    ###########
    nextstep$SAV <- thisstep$SAV * speciesmatrix$overwinter[1]
      
    for (i in 1:numbFPspecies) {
      nextstep$FP[[i]]<-OVERWINTER20(thisstep$FP,i) 
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



######################
# Testing real data  #
######################
# move SAV 
# MOVE_SAV20(LIST[[1]]$SAV,full_threshold=150)
# move FP 
#LIST[[1]]$FP <- lapply(LIST[[1]]$FP,function(x){
#  MOVE_FP20(x)
#})
# Wind FP
#lapply(LIST[[1]]$FP,function(x){
#  WIND20(x,full_threshold=800) 
#})
# Grow SAV
#grow_SAV<-GROW_SAV20(LIST[[1]]$SAV,LIST[[1]]$FPtotal, LIST[[1]]$TOTALP, LIST[[1]]$TOTALN) 
# Grow FP
#for (i in 1:numbFPspecies) {
#  LIST[[1]]$FP[[i]]<-GROW_FP20(LIST[[1]]$FP,LIST[[1]]$FPtotal,i,LIST[[1]]$TOTALP, LIST[[1]]$TOTALN) 
#}
# FPtotal
#LIST[[1]]$FPtotal <- Reduce('+', LIST[[1]]$FP)
######################








