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
# x5... LAND                          #
#                                     #
# Created: MJ McCann 3/23/2014        #
# Updated: Sept 2014 - spatial nutr.  #
# No cap @ 100 g/m2                   #
#######################################
GROW_FP28 <- function(x1,x2,i,x3,x4,x5) { 
  
  new_FP <- matrix(data=0,nrow=height1,ncol=width1)
  
  x1[[i]][x1[[i]]>0 & x5==0] <- x1[[i]][x1[[i]]>0 & x5==0] + # initial biomass 
    
                ((x1[[i]][x1[[i]]>0 & x5==0]*specieslist$maxrgr[i+1]) * # new growth 
        
                (x4[x1[[i]]>0 & x5==0]/1000 / (x4[x1[[i]]>0 & x5==0]/1000 + specieslist$halfsatN[i+1])) * # nitrogen limitation 
    
                (1/(1+specieslist$lightlimitation[i+1]*x2[x1[[i]]>0 & x5==0])) - # biomass limitation 
    
                (lossFP*x1[[i]][x1[[i]]>0 & x5==0])) # biomass loss 
  
  new_FP <- x1[[i]]
  
  #return(x1[[i]])
  
  return(new_FP)
}

#######################
# try with real data 
#######################

#LIST[[1]]$FP[[1]][LIST[[1]]$FP[[1]]>0 & LAND==0] <- LIST[[1]]$FP[[1]][LIST[[1]]$FP[[1]]>0 & LAND==0] + # initial biomass 
  
#  ((LIST[[1]]$FP[[1]][LIST[[1]]$FP[[1]]>0 & LAND==0]*specieslist$maxrgr[2]) * # new growth 
     
#     (LIST[[1]]$TOTALN[LIST[[1]]$FP[[1]]>0 & LAND==0]/1000 / (LIST[[1]]$TOTALN[LIST[[1]]$FP[[1]]>0 & LAND==0]/1000 + specieslist$halfsatN[2])) * # nitrogen limitation 
     
#     (1/(1+specieslist$lightlimitation[2]*LIST[[1]]$FPtotal[LIST[[1]]$FP[[1]]>0 & LAND==0])) - # biomass limitation 
     
#     (lossFP*LIST[[1]]$FP[[1]][LIST[[1]]$FP[[1]]>0 & LAND==0])) # biomass loss 
