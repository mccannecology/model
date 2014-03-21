#############################################################################################
# this will occur in RUNITALL in the real thing, but I need it to make some examples LISTs

# fills  the LIST w/ matrices of 0 
for (i in 1:10){ # 
  LIST[[i]] <- BLANK2()
}
# Start the first time step with some individuals 
LIST[[1]]<-START4() 
#############################################################################################

# I will need to be smarter about indexing each SPmatrix
# I cannot index via: LIST[[i]]$PAmatrix[j,k]

# this is a way I could refer the correct number of SPmatrices 
# should I then assign each one to something else and work on it elsewhere? 
# then I could assign those new values to for(i in 1:numbspecies) LIST[[i+1]][i]
# this does not work because it makes each a list (and not a matrix)
for (i in 1:numbspecies) {
  assign(paste("SP",i,"matrix",sep=""),LIST[[1]][i])
}

# this extracts a matrix (and not a list) out!
matrix(unlist(LIST[[1]][1]),nrow=height,ncol=width)

# these do the same  thing
head(LIST[[1]],1)
LIST[[1]][1]