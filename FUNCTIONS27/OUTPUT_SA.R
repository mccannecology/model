#########################################################
# Outputs model results                                 #
# Replaces functions ANIMATE() and PLOT()               #
#                                                       #
# Compatible with new LIST structure                    #      
# Compatible with SAV component                         #
#                                                       # 
# By: Michael J. McCann                                 #
# Last Updated: 7/8/2014                                #
######################################################### 

########################################
# load workspace for de-bugging
#
# complete LIST 
# 7 years, 125 days each 
# load("testworkspace-complete.Rdata")
########################################

OUTPUT_SA <- function(regimethreshold=70){  
  
  # INDEXING THE DAY BEFORE OVERWINTERING
  # days 
  # 2*days+1
  # 3*days+2
  # 4*days+3
  
  # Make SAV biomass > 100 turn to 100 
  LIST[[days]]$SAV[LIST[[days]]$SAV > 100] <- 100
  LIST[[2*days+1]]$SAV[LIST[[2*days+1]]$SAV > 100] <- 100
  LIST[[3*days+2]]$SAV[LIST[[3*days+2]]$SAV > 100] <- 100
  LIST[[4*days+3]]$SAV[LIST[[4*days+3]]$SAV > 100] <- 100
  
  # Make FP biomass > 100 turn to 100 
  for (m in 1:length(LIST[[1]]$FP)){
    LIST[[days]]$FP[[m]][LIST[[days]]$FP[[m]] > 100] <- 100
    LIST[[2*days+1]]$FP[[m]][LIST[[2*days+1]]$FP[[m]] > 100] <- 100
    LIST[[3*days+2]]$FP[[m]][LIST[[3*days+2]]$FP[[m]] > 100] <- 100
    LIST[[4*days+3]]$FP[[m]][LIST[[4*days+3]]$FP[[m]] > 100] <- 100
  }
    
  # Calculate %SAV cover 
  SAV_end_yr01 <- sum(LIST[[days]]$SAV)/area
  SAV_end_yr02 <- sum(LIST[[2*days+1]]$SAV)/area
  SAV_end_yr03 <- sum(LIST[[3*days+2]]$SAV)/area
  SAV_end_yr04 <- sum(LIST[[4*days+3]]$SAV)/area
  
  # Calculate %FP cover 
  for (m in 1:length(LIST[[1]]$FP)){
    FP_end_yr01 <- sum(LIST[[days]]$FP[[m]])/area
    FP_end_yr02 <- sum(LIST[[2*days+1]]$FP[[m]])/area
    FP_end_yr03 <- sum(LIST[[3*days+2]]$FP[[m]])/area
    FP_end_yr04 <- sum(LIST[[4*days+3]]$FP[[m]])/area
  }
  
  # Calculate TOTAL N
  TOTALN_end_yr01 <- sum(LIST[[days]]$TOTALN)/area
  TOTALN_end_yr02 <- sum(LIST[[2*days+1]]$TOTALN)/area
  TOTALN_end_yr03 <- sum(LIST[[3*days+2]]$TOTALN)/area
  TOTALN_end_yr04 <- sum(LIST[[4*days+3]]$TOTALN)/area
  
  # assign those variables to the environment outside of this function 
  assign("SAV_end_yr01", SAV_end_yr01, pos = 1)
  assign("SAV_end_yr02", SAV_end_yr02, pos = 1)
  assign("SAV_end_yr03", SAV_end_yr03, pos = 1)
  assign("SAV_end_yr04", SAV_end_yr04, pos = 1)
  assign("FP_end_yr01", FP_end_yr01, pos = 1)
  assign("FP_end_yr02", FP_end_yr02, pos = 1)
  assign("FP_end_yr03", FP_end_yr03, pos = 1)
  assign("FP_end_yr04", FP_end_yr04, pos = 1)
  assign("TOTALN_end_yr01", TOTALN_end_yr01, pos = 1)
  assign("TOTALN_end_yr02", TOTALN_end_yr02, pos = 1)
  assign("TOTALN_end_yr03", TOTALN_end_yr03, pos = 1)
  assign("TOTALN_end_yr04", TOTALN_end_yr04, pos = 1)
  
}