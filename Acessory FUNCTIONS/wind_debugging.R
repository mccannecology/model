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

hist(rbeta(10000,2,3))
hist(rbeta(10000,2,4))
hist(rbeta(10000,2,5))
hist(rbeta(10000,2,6))
hist(rbeta(10000,2,7))
hist(rbeta(10000,1.5,3))
hist(rbeta(10000,1.5,4))
hist(rbeta(10000,1.5,5))
hist(rbeta(10000,1.5,6))
hist(rbeta(10000,1.5,7))
hist(rbeta(10000,1.5,8))
hist(rbeta(10000,1.5,9))
hist(rbeta(10000,1.5,20))
hist(rbeta(10000,1,3))
hist(rbeta(10000,1,4))
hist(rbeta(10000,1,5))
hist(rbeta(10000,1,6))
hist(rbeta(10000,1,7))

if (proptomove < 0) {proptomove <- 0} # make any negative values 0 
if (proptomove > 1) {proptomove <- 1} # cannot move >100% of a cell  

nextstep$FP <- lapply(nextstep$FP,function(x){
  WIND21(x,proptomove,direction_of_wind,full_threshold=800) 
})


a <- LIST[[1]]$FP[[1]]

b <- WIND21(a,proptomove,direction_of_wind,full_threshold=800) 
plot(stack(raster(a),raster(b)))
a <- b 

