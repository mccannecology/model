library(ggplot2)

qs <- 0.075
qf <- 0.05

mydata <- matrix(nrow = 1, ncol = 4)

for (i in seq(0.5,10,0.5)){
  for (j in seq(0,300,75)){
    for (k in seq(0,300,75)){
      n <- i / (1 + qs*j + qf*k)
      mydata <- rbind(mydata, c(i,j,k,n))
      
    }
  }
}

# remove NAs 
mydata <- mydata[-1,]
# convert to data.frame 
mydata <- as.data.frame(mydata)
# rename columns 
colnames(mydata) <- c("N","SAV","FP","n")

# plot it 
plot <- ggplot(mydata, aes(x=N,y=n)) + geom_point()
plot <- plot + facet_grid(SAV ~ FP)
plot <- plot + xlab("Maximum Nitrogen (mg/L)")
plot <- plot + ylab("Actual Nitrogen (mg/L)")
plot <- plot + theme_bw(base_size=18)
plot 

ggsave("Scheffer_nutrient_equation.jpg",plot,height=8,width=8)
