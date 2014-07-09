# WIND21() debugging 

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

direction_sample <- c(rep("U",prob_up*100),
                      rep("D",prob_down*100),
                      rep("L",prob_left*100),
                      rep("R",prob_right*100)) # make a vector of wind directions to sample from
direction_of_wind <- sample(direction_sample,1) # sample from it 
proptomove <- rnorm(1,wind_avg,wind_std) # assign an amount to move on the given time-step based on the average & SD of windiness 

# alternatives to normal 
proptomove <- rnorm(1,wind_avg,wind_std) # assign an amount to move on the given time-step based on the average & SD of windiness 

hist(rnorm(1000,wind_avg,wind_std))

jpeg("beta_dist_01.jpg",width=11,heigh=8,units="in",res=1600)
par(mfrow=c(2,5))
hist(rbeta(10000,5,5), sub=paste(c("mean = ", mean(rbeta(10000,5,5))), collapse = ""))
hist(rbeta(10000,5,10), sub=paste(c("mean = ", mean(rbeta(10000,5,10))), collapse = ""))
hist(rbeta(10000,5,20), sub=paste(c("mean = ", mean(rbeta(10000,5,20))), collapse = ""))
hist(rbeta(10000,5,40), sub=paste(c("mean = ", mean(rbeta(10000,5,40))), collapse = ""))
hist(rbeta(10000,5,80), sub=paste(c("mean = ", mean(rbeta(10000,5,80))), collapse = ""))

hist(rbeta(10000,4,4), sub=paste(c("mean = ", mean(rbeta(10000,4,4))), collapse = ""))
hist(rbeta(10000,4,8), sub=paste(c("mean = ", mean(rbeta(10000,4,8))), collapse = ""))
hist(rbeta(10000,4,16), sub=paste(c("mean = ", mean(rbeta(10000,4,16))), collapse = ""))
hist(rbeta(10000,4,32), sub=paste(c("mean = ", mean(rbeta(10000,4,32))), collapse = ""))
hist(rbeta(10000,4,64), sub=paste(c("mean = ", mean(rbeta(10000,4,64))), collapse = ""))
dev.off()

jpeg("beta_dist_02.jpg",width=11,heigh=8,units="in",res=1600)
par(mfrow=c(2,5))
hist(rbeta(10000,3,3), sub=paste(c("mean = ", mean(rbeta(10000,3,3))), collapse = ""))
hist(rbeta(10000,3,6), sub=paste(c("mean = ", mean(rbeta(10000,3,6))), collapse = ""))
hist(rbeta(10000,3,12), sub=paste(c("mean = ", mean(rbeta(10000,3,12))), collapse = ""))
hist(rbeta(10000,3,24), sub=paste(c("mean = ", mean(rbeta(10000,3,24))), collapse = ""))
hist(rbeta(10000,3,48), sub=paste(c("mean = ", mean(rbeta(10000,3,48))), collapse = ""))

hist(rbeta(10000,2,2), sub=paste(c("mean = ", mean(rbeta(10000,2,2))), collapse = ""))
hist(rbeta(10000,2,4), sub=paste(c("mean = ", mean(rbeta(10000,2,4))), collapse = ""))
hist(rbeta(10000,2,8), sub=paste(c("mean = ", mean(rbeta(10000,2,8))), collapse = ""))
hist(rbeta(10000,2,16), sub=paste(c("mean = ", mean(rbeta(10000,2,16))), collapse = ""))
hist(rbeta(10000,2,32), sub=paste(c("mean = ", mean(rbeta(10000,2,32))), collapse = ""))
dev.off()

jpeg("beta_dist_03.jpg",width=11,heigh=8,units="in",res=1600)
par(mfrow=c(2,5))
hist(rbeta(10000,1.5,1.5), sub=paste(c("mean = ", mean(rbeta(10000,1.5,1.5))), collapse = ""))
hist(rbeta(10000,1.5,3), sub=paste(c("mean = ", mean(rbeta(10000,1.5,3))), collapse = ""))
hist(rbeta(10000,1.5,6), sub=paste(c("mean = ", mean(rbeta(10000,1.5,6))), collapse = ""))
hist(rbeta(10000,1.5,12), sub=paste(c("mean = ", mean(rbeta(10000,1.5,12))), collapse = ""))
hist(rbeta(10000,1.5,24), sub=paste(c("mean = ", mean(rbeta(10000,1.5,24))), collapse = ""))

hist(rbeta(10000,1,1), sub=paste(c("mean = ", mean(rbeta(10000,1,1))), collapse = ""))
hist(rbeta(10000,1,2), sub=paste(c("mean = ", mean(rbeta(10000,1,2))), collapse = ""))
hist(rbeta(10000,1,4), sub=paste(c("mean = ", mean(rbeta(10000,1,4))), collapse = ""))
hist(rbeta(10000,1,8), sub=paste(c("mean = ", mean(rbeta(10000,1,8))), collapse = ""))
hist(rbeta(10000,1,16), sub=paste(c("mean = ", mean(rbeta(10000,1,16))), collapse = ""))
dev.off()

jpeg("beta_dist_04.jpg",width=11,heigh=8,units="in",res=1600)
par(mfrow=c(2,5))
hist(rbeta(10000,0.5,0.5), sub=paste(c("mean = ", mean(rbeta(10000,0.5,0.5))), collapse = ""))
hist(rbeta(10000,0.5,1), sub=paste(c("mean = ", mean(rbeta(10000,0.5,1))), collapse = ""))
hist(rbeta(10000,0.5,2), sub=paste(c("mean = ", mean(rbeta(10000,0.5,2))), collapse = ""))
hist(rbeta(10000,0.5,4), sub=paste(c("mean = ", mean(rbeta(10000,0.5,4))), collapse = ""))
hist(rbeta(10000,0.5,8), sub=paste(c("mean = ", mean(rbeta(10000,0.5,8))), collapse = ""))

hist(rbeta(10000,0.1,0.1), sub=paste(c("mean = ", mean(rbeta(10000,0.1,0.1))), collapse = ""))
hist(rbeta(10000,0.1,0.2), sub=paste(c("mean = ", mean(rbeta(10000,0.1,0.2))), collapse = ""))
hist(rbeta(10000,0.1,0.4), sub=paste(c("mean = ", mean(rbeta(10000,0.1,0.4))), collapse = ""))
hist(rbeta(10000,0.1,0.8), sub=paste(c("mean = ", mean(rbeta(10000,0.1,0.8))), collapse = ""))
hist(rbeta(10000,0.1,1.6), sub=paste(c("mean = ", mean(rbeta(10000,0.1,1.6))), collapse = ""))
dev.off()

if (proptomove < 0) {proptomove <- 0} # make any negative values 0 
if (proptomove > 1) {proptomove <- 1} # cannot move >100% of a cell  

nextstep$FP <- lapply(nextstep$FP,function(x){
  WIND21(x,proptomove,direction_of_wind,full_threshold=800) 
})


a <- LIST[[1]]$FP[[1]]

b <- WIND21(a,proptomove,direction_of_wind,full_threshold=800) 
plot(stack(raster(a),raster(b)))
a <- b 

