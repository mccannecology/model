#############################################################################################
# this will occur in RUNITALL in the real thing, but I need it to make some examples LISTs

# Create a blank LIST 
LIST <- vector("list",10) # Creates the "blank" LIST 

# fill  the LIST w/ matrices of 0 
for (i in 1:10){ # 
  LIST[[i]] <- BLANK2()
}

# Start the first time step with some individuals 
LIST[[1]]<-START4() 
#############################################################################################



#############################################################################################
# I will need to be smarter about indexing each SPmatrix
# I cannot index via: LIST[[i]]$PAmatrix[j,k]

# extract each SPmatrix from list and assign to an object in the environment
for (i in 1:numbspecies) {
  assign(paste("SP",i,"matrix",sep=""),matrix(unlist(LIST[[1]][1]),nrow=height,ncol=width)) #use unlist() to pull matrix out of list
}

# these are the actions that I am going to do for each SPmatrix 
# here, I will do it just on SP1matrix 
for (j in 1:height) { # loop over all rows (height)
  for (k in 1:width) { # loop over all columns (width)
    if (LIST[[i]]$SP1matrix[j,k] > 0) {
      LIST[[i+1]]$SP1matrix[j,j] <- LIST[[i]]$SP1matrix[j,k] + 1 # This is where the growth formula goes 
    }  
  }   
}

# POSSIBLE SOLUTION: if statement - check numbspecies - based on that carry out different set of actions 
# Do actions to the SP1matrix 
if (numbspecies == 4) {}
else if (numbspecies == 3) {}
else if (numbspecies == 2) {}
else if (numbspecies == 1) {}


# The input needs to be both LIST[[i]]$SP1matrix and LIST[[i+1]]$SP1matrix
# Then I can index things properly
GROW <- function(x1,x2) {
  for (j in 1:height) { # loop over all rows (height)
    for (k in 1:width) { # loop over all columns (width)
      if (x1[j,k] > 0) {
        x2[j,k] <- x1[j,k] + 1 # This is where the growth formula goes 
      }  
    }   
  }
  RESULT <- x2
  return(RESULT) 
}

# Testing the function GROW()
LIST[[2]]$SP1matrix <- GROW(LIST[[1]]$SP1matrix, LIST[[2]]$SP1matrix) 
LIST[[2]]$SP2matrix <- GROW(LIST[[1]]$SP2matrix, LIST[[2]]$SP2matrix) 
LIST[[2]]$SP3matrix <- GROW(LIST[[1]]$SP2matrix, LIST[[2]]$SP2matrix) 
