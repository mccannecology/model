#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 4/24/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("input07and08.csv") # imports parameter  values for all simulations 
head(data)
data_smallFPeffect <- data[1:90,]
data_largeFPeffect <- data[91:180,]

# If I have multiple simulations @ the same parameter values, then I need to get summary means, variances (library dplyr) & plot those instead

# Y = proportion years w/ avg. FP cover > 70%
a <- ggplot(data, aes(x=TOTALN,y=propyears_avgFPcover_abovethreshold)) + geom_point(position="jitter")
a

# Y = proportion years w/ >50% days w/ FP cover > 70%
b <- ggplot(data, aes(x=TOTALN,y=propyears_prop_daysFP_abovehalf)) + geom_point(position="jitter")
b

# Y = average average FP cover for all years (except for first three)
# Lumping all simulations together 
c <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover)) + geom_point()
c

d <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover)) + geom_point()
d

# Y = average average FP cover for all years (except for first three)
# small FP effect 
# light limitation (SAV on SAV): a = 0.01
# FP shading SAV: b = 0.02
e <- ggplot(data_smallFPeffect, aes(x=TOTALN,y=avg_avg_FPcover,colour=initial_perc_FP_cover)) + geom_point(position="jitter")
e

f <- ggplot(data_smallFPeffect, aes(x=TOTALN,y=avg_avg_SAVcover,colour=initial_perc_SAV_cover)) + geom_point(position="jitter")
f

# Y = average average FP cover for all years (except for first three)
# large FP effect
# light limitation (SAV on SAV): a = 0.02
# FP shading SAV: b = 0.04
g <- ggplot(data_largeFPeffect, aes(x=TOTALN,y=avg_avg_FPcover,colour=initial_perc_FP_cover)) + geom_point(position="jitter")
g <- g + ylab("% FP cover")
g <- g + labs(colour="Initial % FP")
g <- g + theme_classic(base_size=18)
g

h <- ggplot(data_largeFPeffect, aes(x=TOTALN,y=avg_avg_SAVcover,colour=initial_perc_SAV_cover)) + geom_point(position="jitter")
h <- h + ylab("% SAV cover")
h <- h + labs(colour="Initial % SAV")
h <- h + theme_classic(base_size=18)
h

grid.arrange(g,h,ncol=1)
