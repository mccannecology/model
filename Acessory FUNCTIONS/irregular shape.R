# short (3 yrs, 50 days each)
# one FP species
load("testworkspace-1FPspecies.Rdata")

LIST[[1]]$SAV

LAND <- LIST[[1]]$SAV

# land 
LAND[1:5,1:5] <- 1
# water 
LAND[6:nrow(LAND),6:ncol(LAND)] <- 0
LAND[1:5,6:ncol(LAND)] <- 0
LAND[6:nrow(LAND),1:5] <- 0

# set land cells in plant matrices to 0 
LIST[[1]]$SAV[1:5,1:5] <- 0
LIST[[1]]$FPtotal[1:5,1:5] <- 0
LIST[[1]]$FP[[1]][1:5,1:5] <- 0

height1 <- 20
height2 <- 3
width1 <- 20
width2 <- 3

shape <- "hook"
LAND <- matrix(0, height1, width1)
LAND[1:height2,1:width2] <- 1 # top left cells equal 1 
LAND

shape <- "eight"
LAND <- matrix(0, height1, width1)
LAND[1:height2,1:width2] <- 1 # top left cells equal 1
LAND[(height1-height2):height1,(width1-width2+1):width1] <- 1 # bottom right cells equal 1 
LAND

shape <- "tee"
LAND <- matrix(0, height1, width1)
LAND[1:height2,1:width2] <- 1  # top left cells equal 1
LAND[1:height2,(width1-width2+1):width1] <- 1 # top right cells equal 1
LAND

shape <- "cross"
LAND <- matrix(0, height1, width1)
LAND[1:height2,1:width2] <- 1 # top left cells equal 1
LAND[1:height2,(width1-width2+1):width1] <- 1 # top right cells equal 1
LAND[(height1-height2):height1,(width1-width2+1):width1] <- 1 # bottom right cells equal 1 
LAND[(height1-height2+1):height1,1:width2] <- 1 # bottom left cells equal 1
LAND


# testing WIND25()

direction_sample <- c(rep("U",prob_up*100),
                      rep("D",prob_down*100),
                      rep("L",prob_left*100),
                      rep("R",prob_right*100)) # make a vector of wind directions to sample from

direction_of_wind <- sample(direction_sample,1) # sample from it 

proptomove <- rbeta(1,wind_shape1,wind_shape2) # assign an amount to move on the given time-step based on a beta distribution

if (proptomove < 0) {proptomove <- 0} # make any negative values 0 

if (proptomove > 1) {proptomove <- 1} # cannot move >100% of a cell  

temp <- lapply(LIST[[1]]$FP,function(x){
  WIND25(x,proptomove,direction_of_wind,full_thresh_wind=800) 
})

