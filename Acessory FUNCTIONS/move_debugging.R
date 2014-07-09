# MOVE21() debugging 

##########################################
# load workspace for de-bugging
# 
# LIST has an initial time step only 
# short (3 yrs, 50 days each)
# one FP species
# load("testworkspace.Rdata") 
# two FP species
# load("testworkspace-2FPspecies.Rdata") 
# four FP species
# load("testworkspace-4FPspecies.Rdata") 
##########################################

a<-LIST[[1]]$FP[[1]]
b<-LIST[[1]]$FP[[2]]
c<-stack(raster(a),raster(b))
plot(c)

d <- LIST[[1]]$FP  


e <- lapply(d,function(x){
  MOVE21(x,neigh_thresh_FP,focal_thresh_FP,amnt_colonize_FP)
})
d_rast <-stack(raster(d[[1]]),raster(d[[2]]))
e_rast <- stack(raster(e[[1]]),raster(e[[2]]))
plot(stack(d_rast,e_rast))
d <- e 
