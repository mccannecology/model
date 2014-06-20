# examing the FP & SAV growth functions 

# define the parameters here 
maxrgr <- 0.5 
halfsatP <- 0.2
halfsatN <- 0.5 
halfsatB <- 42 
N <- 0.01 
P <- N/4  
loss <- 0.05

FP_GROW <- function(x,y) {
  y <- x + (maxrgr*x)*(N/(N+halfsatN))*(P/(P+halfsatP))*(x/(x+halfsatB))-(loss*x)
  return(y)
}

# This is linear 
# There is no nutrient uptake
temp<-matrix(data=rep.int(0,1000),nrow=1000,ncol=2)
temp[,1] <- seq(1,1000,1)
for (i in 1:1000) {
  y <- FP_GROW(i)
  temp[i,2] <- y
}
plot(temp[,1],temp[,2],ylab="New growth",xlab="Biomass_t")
abline(a=0,b=1)

