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




for (i in 1:10000){ 
  LIST[[i]] <- BLANK2()
}



