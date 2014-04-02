#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
######################################################### 

library(ggplot2)

data <- read.csv("output06b.csv") # imports parameter  values for all simulations 


# If I have multiple simulations @ the same parameter values, then I need to get summary means, variances (library dplyr) & plot those instead

# Y = proportion years w/ avg. FP cover > 70%
a <- ggplot(data, aes(x=area,y=propyears_avgFP_abovethreshold,shape=factor(numbspecies),colour=factor(HtoW))) + geom_point(position="jitter")
a <- a + facet_grid(speciesdiffs ~ wind)
a

# Y = proportion years w/ >50% days w/ FP cover > 70%
b <- ggplot(data, aes(x=area,y=propyears_propdaysFP_abovehalf,shape=factor(numbspecies),colour=factor(HtoW))) + geom_point(position="jitter")
b <- b + facet_grid(speciesdiffs ~ wind)
b

# Y = proportion years w/ avg. FP cover > 70%
# subset - data 3 times - HtoW = 4, 1, or 0.25 
# Rectangle: 4:1 H:W
a_HtoW4 <- ggplot(subset(data, data$HtoW == 4), aes(x=area,y=propyears_avgFP_abovethreshold,colour=factor(numbspecies))) + geom_line() + geom_point()
a_HtoW4 <- a_HtoW4 + facet_grid(speciesdiffs ~ wind)
a_HtoW4 <- a_HtoW4 + ylab("proportion years w/ avg. FP cover > 70%")
a_HtoW4 <- a_HtoW4 + labs(title="Rectangle: 4:1 H:W")
a_HtoW4
# if you want to make it B&W 
a_HtoW4 <- a_HtoW4 + scale_colour_grey(start = 0, end = .8) + theme_bw()

# Rectangle: 1:4 H:W
a_HtoW0_25 <- ggplot(subset(data, data$HtoW == 0.25), aes(x=area,y=propyears_avgFP_abovethreshold,colour=factor(numbspecies))) + geom_line() + geom_point()
a_HtoW0_25 <- a_HtoW0_25 + facet_grid(speciesdiffs ~ wind)
a_HtoW0_25 <- a_HtoW0_25 + ylab("proportion years w/ avg. FP cover > 70%")
a_HtoW0_25 <- a_HtoW0_25 + labs(title="Rectangle: 1:4 H:W")
a_HtoW0_25
# if you want to make it B&W 
a_HtoW0_25 <- a_HtoW0_25 + scale_colour_grey(start = 0, end = .8) + theme_bw()

# Square
a_HtoW1 <- ggplot(subset(data, data$HtoW == 1), aes(x=area,y=propyears_avgFP_abovethreshold,colour=factor(numbspecies))) + geom_line() + geom_point()
a_HtoW1 <- a_HtoW1 + facet_grid(speciesdiffs ~ wind)
a_HtoW1 <- a_HtoW1 + ylab("proportion years w/ avg. FP cover > 70%")
a_HtoW1 <- a_HtoW1 + labs(title="Square")
a_HtoW1
# if you want to make it B&W 
a_HtoW1 <- a_HtoW1 + scale_colour_grey(start = 0, end = .8) + theme_bw()


# Y = proportion years w/ >50% days w/ FP cover > 70%
# subset - data 3 times - HtoW = 4, 1, or 0.25 
# Rectangle: 4:1 H:W
b_HtoW4 <- ggplot(subset(data, data$HtoW == 4), aes(x=area,y=propyears_propdaysFP_abovehalf,colour=factor(numbspecies))) + geom_line() + geom_point()
b_HtoW4 <- b_HtoW4 + facet_grid(speciesdiffs ~ wind)
b_HtoW4 <- b_HtoW4 + ylab("proportion years w/ >50% days w/ FP cover > 70%")
b_HtoW4 <- b_HtoW4 + labs(title="Rectangle: 4:1 H:W")
b_HtoW4
# if you want to make it B&W 
b_HtoW4 <- b_HtoW4 + scale_colour_grey(start = 0, end = .8) + theme_bw()

# Rectangle: 1:4 H:W
b_HtoW0_25 <- ggplot(subset(data, data$HtoW == 0.25), aes(x=area,y=propyears_propdaysFP_abovehalf,colour=factor(numbspecies))) + geom_line() + geom_point()
b_HtoW0_25 <- b_HtoW0_25 + facet_grid(speciesdiffs ~ wind)
b_HtoW0_25 <- b_HtoW0_25 + ylab("proportion years w/ >50% days w/ FP cover > 70%")
b_HtoW0_25 <- b_HtoW0_25 + labs(title="Rectangle: 1:4 H:W")
b_HtoW0_25
# if you want to make it B&W 
b_HtoW0_25 <- b_HtoW0_25 + scale_colour_grey(start = 0, end = .8) + theme_bw()

# Square
b_HtoW1 <- ggplot(subset(data, data$HtoW == 1), aes(x=area,y=propyears_propdaysFP_abovehalf,colour=factor(numbspecies))) + geom_line() + geom_point()
b_HtoW1 <- b_HtoW1 + facet_grid(speciesdiffs ~ wind)
b_HtoW1 <- b_HtoW1 + ylab("proportion years w/ >50% days w/ FP cover > 70%")
b_HtoW1 <- b_HtoW1 + labs(title="Square")
b_HtoW1
# if you want to make it B&W 
b_HtoW1 <- b_HtoW1 + scale_colour_grey(start = 0, end = .8) + theme_bw()

