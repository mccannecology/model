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
