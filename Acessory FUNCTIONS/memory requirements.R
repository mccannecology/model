####################################################################
# The effect of LIST size on memory requirements / object size
####################################################################

SIZE <- NULL 
SIZE <- data.frame(HEIGHT=0,WIDTH=0,LENGTH=0,MEMORY=0)

for (i in seq(1,2001,500)) {
 LIST <- vector("list",i) # Creates the "blank" LIST   
  
  for (j in c(10,250,500)){
    for (k in c(10,250,500)){
      
      height <- j 
      width <- k
      
      # fill  the LIST w/ matrices of 0 
      for (m in 1:length(LIST)){ 
        LIST[[m]] <- BLANK2()  
      }
 
      SIZE <- rbind(SIZE,c(height,width,length(LIST),object.size(LIST))) 
    }
  }
}

# amount of memory currently in use by the OS - units: MB
memory.size(max=FALSE)

# maximum amount of memory from the OS - units: MB
memory.size(max=TRUE)

# current memory limit - units: MB
memory.limit(size=NA)

# set a new memory limit - units: MB
memory.limit(size=6000)

