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
source("RUNITALL20.R")
Rprof()
summaryRprof(tmp)
unlink(tmp)

# profiling a function
Rprof("temp.out")
source("RUNITALL20.R")
Rprof()
summaryRprof("temp.out")

# profiling a function
Rprof("temp.out")
source("testfunction.R")
Rprof()
summaryRprof("temp.out")

Rprof(tmp <- tempfile())
a<-testfunction()
Rprof()
summaryRprof(tmp)
unlink(tmp)






# using lapply and an inline function on a LIST 
LIST<-paste()
lapply(LIST,function(x){
  FP1matrix
})

