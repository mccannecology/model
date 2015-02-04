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
# x3... LIST[[i]]$TOTALN              #
# x4... LAND                          #
#                                     #
# Created: MJ McCann 3/23/2014        #
# Updated: Sept 2014 - spatial nutr.  #
# No cap @ 100 g/m2                   #
#######################################
GROW_FP30 <- function(x1,x2,i,x3,x4) { 
  
  new_FP <- matrix(data=0,nrow=height1,ncol=width1)
  
  x1[[i]][x1[[i]]>0 & x4==0] <- x1[[i]][x1[[i]]>0 & x4==0] + # initial biomass 
    
                ((x1[[i]][x1[[i]]>0 & x4==0]*specieslist$maxrgr[i+1]) * # new growth 
        
                (x3[x1[[i]]>0 & x4==0]/1000 / (x3[x1[[i]]>0 & x4==0]/1000 + specieslist$halfsatN[i+1])) * # nitrogen limitation 
    
                (1/(1+specieslist$lightlimitation[i+1]*x2[x1[[i]]>0 & x4==0])) - # biomass limitation 
    
                (lossFP*x1[[i]][x1[[i]]>0 & x4==0])) # biomass loss 
  
  new_FP <- x1[[i]]
  
  return(new_FP)
}

#######################
# try with real data 
#######################
