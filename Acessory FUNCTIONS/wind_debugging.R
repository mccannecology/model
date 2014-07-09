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
proptomove <- rbeta(1,wind_shape1,wind_shape2) # assign an amount to move on the given time-step based on a beta distribution

if (proptomove < 0) {proptomove <- 0} # make any negative values 0 
if (proptomove > 1) {proptomove <- 1} # cannot move >100% of a cell  

nextstep$FP <- lapply(nextstep$FP,function(x){
  WIND21(x,proptomove,direction_of_wind,full_thres_wind=800) 
})

direction_of_wind <- "U"
direction_of_wind <- "D"
direction_of_wind <- "L"
direction_of_wind <- "R"
proptomove <- 1 
proptomove <- 0.99
proptomove <- 0.55
proptomove <- 0.75
proptomove <- 0.95
proptomove <- 0.1

a <- LIST[[1]]$FP[[1]]

b <- WIND22(a,proptomove,direction_of_wind,full_thresh_wind=800) 
plot(stack(raster(a),raster(b)))
a <- b 


a <- LIST[[1]]$FP[[1]]

b <- WIND22(a,proptomove,direction_of_wind,full_thresh_wind=800) 
plot(stack(raster(a),raster(b)))
a <- b 




x1 <- LIST[[1]]$FP[[1]]



for (j in 1:height) { # loop over all rows (height)
  for (k in 1:width) { # loop over all columns (width)
    print(c(j,k))
  }
}
