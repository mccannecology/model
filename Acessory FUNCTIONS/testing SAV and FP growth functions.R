# examing the FP & SAV growth functions 

# define the parameters here 
maxrgr <- 0.5 
halfsatP <- 0.2
halfsatN <- 0.5 
halfsatB <- 42 
N <- 2 
P <- 0.5 
loss <- 0.05

FP_GROW <- function(x,y) {
  y <- x + (maxrgr*x)*(N/(N+halfsatN))*(P/(P+halfsatP))*(x/(x+halfsatB))-(loss*x)
  return(y)
}

# This is linear 
# There is no nutrient uptake
temp<-0
for (i in 1:1000) {
  y <- FP_GROW(i)
  temp <- c(temp, y)
}
plot(temp)

