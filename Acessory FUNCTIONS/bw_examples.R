# using lapply and an inline function on a LIST 
a<-matrix(5,5,data=rnorm(25))
d<-matrix(5,5,data=rnorm(25))

b<-list(a,a,a,a,a)
j<-list(d,d,d,d,d)

LIST<-list(b,j,b,j,b)

sapply(LIST,function(x){
  sapply(x,sum)
})

# profiling a function
Rprof(tmp <- tempfile())
source("RUNITALL5.R")
Rprof()
summaryRprof(tmp)
unlink(tmp)


Rprof(tmp <- tempfile())
source("testfunction.R")
Rprof()
summaryRprof(tmp)
unlink(tmp)



# using lapply and an inline function on a LIST 
LIST<-paste()
lapply(LIST,function(x){
  FP1matrix
})

