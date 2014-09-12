# trouble-shooting spatially-explicit nutrient uptake 


# assign the current time step to "thisstep"
thisstep <- LIST[[1]]
nextstep <- LIST[[1]]

############
# Grow SAV #
############ 
nextstep$SAV <- GROW_SAV26(thisstep$SAV,
                           thisstep$FPtotal,
                           thisstep$TOTALP,
                           thisstep$TOTALN,
                           LAND) 

###########
# Grow FP #
###########  
for (j in 1:numbFPspecies) {
  nextstep$FP[[j]]<-GROW_FP26(thisstep$FP,
                              thisstep$FPtotal,
                              j,
                              thisstep$TOTALP,
                              thisstep$TOTALN,
                              LAND) 
}

##########
# Sum FP #
##########
nextstep$FPtotal <- Reduce('+', nextstep$FP)

############
# Uptake N #
############
nextstep$TOTALN <- UPTAKE_N26(thisstep,nextstep)

############
# Uptake P #
############ 
nextstep$TOTALP <- UPTAKE_P26(thisstep,nextstep)

# assign the result ("nextstep") to the next time step
thisstep <- nextstep 


####################
# plot the results #
####################
plot(stack(raster(thisstep$SAV),raster(nextstep$SAV)))

plot(stack(raster(thisstep$FPtotal),raster(nextstep$FPtotal)))

plot(stack(raster(thisstep$TOTALN),raster(nextstep$TOTALN)))

plot(stack(raster(thisstep$TOTALP),raster(nextstep$TOTALP)))

# assign the result ("nextstep") to the next time step
thisstep <- nextstep 
# then re-start from the top 
