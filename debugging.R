#############################################################################################
# Make some examples LISTs
# occurs in RUNITALL in the real thing

# Create a blank LIST 
LIST <- vector("list",10) # Creates the "blank" LIST 

# fill  the LIST w/ matrices of 0 
for (i in 1:10){ # 
  LIST[[i]] <- BLANK2()
}

# Start the first time step with some individuals 
LIST[[1]]<-START4() 
#############################################################################################

# Test function GROW() on individual matrices 
LIST[[2]]$SP1matrix <- GROW(LIST[[1]]$SP1matrix, LIST[[2]]$SP1matrix) 
LIST[[2]]$SP2matrix <- GROW(LIST[[1]]$SP2matrix, LIST[[2]]$SP2matrix) 
LIST[[2]]$SP3matrix <- GROW(LIST[[1]]$SP2matrix, LIST[[2]]$SP2matrix) 

# Test function STEP10() on the LIST 
LIST<-STEP10()


# playing around with some forms of the growth formula for GROW()

# define parameters 
# will use speciesmatrix indexing in the future 
maxrgr<-0.4
halfsatP<-0.05
halfsatN<-0.04
halfsatB<-42
TOTALN<-2
TOTALP<-0.5
loss<-0.05

LIST[[1]]$SP1matrix
LIST[[1]]$SP1matrix[10,2]
LIST[[1]]$SP1matrix[10,2] + maxrgr*LIST[[1]]$SP1matrix[10,2]*(halfsatB/(LIST[[1]]$SP1matrix[10,2]+halfsatB))*
  ((TOTALP/(TOTALP+halfsatP))*(TOTALN/(TOTALN+halfsatN))) -
  (loss*LIST[[1]]$SP1matrix[10,2])
