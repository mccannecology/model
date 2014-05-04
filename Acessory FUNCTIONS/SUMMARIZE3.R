#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 5/04/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output10.csv") # imports parameter  values for all simulations 
head(data)

# If I have multiple simulations @ the same parameter values, then I need to get summary means, variances (library dplyr) & plot those instead

###################
# small FP effect #
###################
# Y = average average FP cover for all years (except for first three)
#
# light limitation (SAV on SAV): a = 0.01
# FP shading SAV: b = 0.02
e <- ggplot(data, aes(x=TOTALN,y=TOTALP,colour=avg_avg_FPcover)) + geom_point(position="jitter")
e <- e + xlab("Total N (mg/L)")
e <- e + ylab("Total P (mg/L)")
e <- e + labs(colour="% FP cover")
e <- e + theme_classic(base_size=18)
e
f <- ggplot(data, aes(x=TOTALN,y=TOTALP,colour=avg_avg_SAVcover)) + geom_point(position="jitter")
f <- f + xlab("Total N (mg/L)")
f <- f + ylab("Total P (mg/L)")
f <- f + labs(colour="% SAV cover")
f <- f + theme_classic(base_size=18)
f

e_f <- grid.arrange(e,f,ncol=1, main="Small FP effect",sub="a=0.01, b=0.02")
e_f <- arrangeGrob(e,f,ncol=1, main="Small FP effect",sub="a=0.01, b=0.02")
ggsave(file="N vs P - small FP effect.pdf",e_f, height=11,width=8)

