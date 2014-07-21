load("testworkspace-1FPspecies.Rdata") 

LIST[[1]]$SAV[1:5,1:5]<-"x"
LIST[[1]]$FP[[1]][1:5,1:5]<-"x"
LIST[[1]]$FPtotal[1:5,1:5]<-"x"

# now everything is a character 
class(LIST[[1]]$SAV[1,1])
class(LIST[[1]]$SAV[20,20])

# use as.numeric to get the # back
as.numeric(LIST[[1]]$SAV[20,20])

temp <- as.data.frame(LIST[[1]]$SAV)
temp[1,6]
class(temp[1,3])
