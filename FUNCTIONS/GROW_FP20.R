#######################################
# Growth function                     #
# Compatible w/ new LIST structure    #
# Compatible w/ SAV component         #
#                                     #
# Identical to Scheffer et al. 2003   # 
#                                     #
# INPUTS:                             #
# x1... LIST[[i]]$FP                  #
# x2... LIST[[i]]$FPtotal             #
# i...  numbFPspecies                 # 
# x3... LIST[[i]]$TOTALP              #
# x4... LIST[[i]]$TOTALN              #
#                                     #
# Created: MJ McCann 3/23/2014        #
# Updated: 6/20/2014                  #
# No cap @ 100 g/m2                   #
#######################################
GROW_FP20 <- function(x1,x2,i,x3,x4) { 
  x1[[i]][x1[[i]]>0] <- x1[[i]][x1[[i]]>0] + # initial biomass 
    
                ((x1[[i]][x1[[i]]>0]*speciesmatrix$maxrgr[i+1]) * # new growth 
        
                (x4/(x4+speciesmatrix$halfsatN[i+1])) * # nitrogen limitation 
    
                (1/(1+lightlimitation_FP*x2[x1[[i]]>0])) - # biomass limitation 
    
                (lossFP*x1[[i]][x1[[i]]>0])) # biomass loss 
  
  return(x1[[i]]) 
}




# try with real numbers
#LIST[[1]]$FP[[i]][LIST[[1]]$FP[[i]]>0] <- LIST[[1]]$FP[[i]][LIST[[1]]$FP[[i]]>0] + # initial biomass 
  
#  ((LIST[[1]]$FP[[i]][LIST[[1]]$FP[[i]]>0]*speciesmatrix$maxrgr[i+1]) * # new growth 
     
#     (LIST[[1]]$TOTALN/(LIST[[1]]$TOTALN+speciesmatrix$halfsatN[i+1])) * # nitrogen limitation 
     
#     (1/(1+lightlimitation_FP*LIST[[1]]$FPtotal[LIST[[1]]$FP[[i]]>0])) - # biomass limitation 
     
#     (lossFP*LIST[[1]]$FP[[i]][LIST[[1]]$FP[[i]]>0])) # biomass loss 

#new_FP <- LIST[[1]]$FP[[i]]

#return(new_FP) 
