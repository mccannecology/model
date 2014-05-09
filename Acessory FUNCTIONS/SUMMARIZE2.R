#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 4/24/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output10-11-12.csv") # imports parameter  values for all simulations 
head(data)
data_smallFPeffect <- data[1:450,]
data_medFPeffect <- data[451:900,]
data_largeFPeffect <- data[901:1350,]

# If I have multiple simulations @ the same parameter values
# And I want to plot a single point @ a given nutrient level 
# then I need to get summary means, variances (library dplyr) & plot those instead

###################
# small FP effect #
###################
# Y = average average FP cover for all years (except for first three)
#
# light limitation (SAV on SAV): a = 0.01
# FP shading SAV: b = 0.02
e <- ggplot(data, aes(x=TOTALN,y=TOTALP,colour=avg_avg_FPcover))
e <- e + geom_jitter(position = position_jitter(width=0.2,height=0.04)) #1:5 aspect ratio
e <- e + xlab("Total N (mg/L)")
e <- e + ylab("Total P (mg/L)")
e <- e + labs(colour="% FP cover")
e <- e + theme_classic(base_size=18)
e <- e + scale_colour_gradient(limits=c(0,100))
e
f <- ggplot(data, aes(x=TOTALN,y=TOTALP,colour=avg_avg_SAVcover))
f <- f + geom_jitter(position = position_jitter(width=0.2,height=0.04)) #1:5 aspect ratio
f <- f + xlab("Total N (mg/L)")
f <- f + ylab("Total P (mg/L)")
f <- f + labs(colour="% SAV cover")
f <- f + theme_classic(base_size=18)
f <- f + scale_colour_gradient(limits=c(0,100))
f

e_f <- grid.arrange(e,f,ncol=1, main="Small FP effect",sub="a=0.01, b=0.02")
e_f <- arrangeGrob(e,f,ncol=1, main="Small FP effect",sub="a=0.01, b=0.02")
ggsave(file="N vs P - small FP effect.pdf",e_f, height=11,width=8)


#################
# med FP effect #
#################
# Y = average average FP cover for all years (except for first three)
# light limitation (SAV on SAV): a = 0.02
# FP shading SAV: b = 0.04
g <- ggplot(data_medFPeffect, aes(x=TOTALN,y=TOTALP,colour=avg_avg_FPcover)) 
g <- g + geom_jitter(position = position_jitter(width=0.2,height=0.04)) #1:5 aspect ratio
g <- g + xlab("Total N (mg/L)")
g <- g + ylab("Total P (mg/L)")
g <- g + labs(colour="% FP cover")
g <- g + theme_classic(base_size=18)
g <- g + scale_colour_gradient(limits=c(0,100))
g

h <- ggplot(data_medFPeffect, aes(x=TOTALN,y=TOTALP,colour=avg_avg_SAVcover))
h <- h + geom_jitter(position = position_jitter(width=0.2,height=0.04)) #1:5 aspect ratio
h <- h + xlab("Total N (mg/L)")
h <- h + ylab("Total P (mg/L)")
h <- h + labs(colour="% SAV cover")
h <- h + theme_classic(base_size=18)
h <- h + scale_colour_gradient(limits=c(0,100))
h

g_h <- grid.arrange(g,h,ncol=1, main="Medium FP effect",sub="a=0.02, b=0.04")
g_h <- arrangeGrob(g,h,ncol=1, main="Medium FP effect",sub="a=0.02, b=0.04")
ggsave(file="N vs P - med FP effect.pdf",g_h, height=11,width=8)

###################
# large FP effect #
###################
# Y = average average FP cover for all years (except for first three)
# light limitation (SAV on SAV): a = 0.04
# FP shading SAV: b = 0.08
i <- ggplot(data_largeFPeffect, aes(x=TOTALN,y=TOTALP,colour=avg_avg_FPcover)) 
i <- i + geom_jitter(position = position_jitter(width=0.2,height=0.04)) #1:5 aspect ratio
i <- i + xlab("Total N (mg/L)")
i <- i + ylab("Total P (mg/L)")
i <- i + labs(colour="% FP cover")
i <- i + theme_classic(base_size=18)
i <- i + scale_colour_gradient(limits=c(0,100))
i

j <- ggplot(data_largeFPeffect, aes(x=TOTALN,y=TOTALP,colour=avg_avg_SAVcover)) 
j <- j + geom_jitter(position = position_jitter(width=0.2,height=0.04)) #1:5 aspect ratio
j <- j + xlab("Total N (mg/L)")
j <- j + ylab("Total P (mg/L)")
j <- j + labs(colour="% SAV cover")
j <- j + theme_classic(base_size=18)
j <- j + scale_colour_gradient(limits=c(0,100))
j

i_j <- grid.arrange(i,j,ncol=1,main="Large FP effect",sub="a=0.04, b=0.08")
i_j <- arrangeGrob(i,j,ncol=1, main="Large FP effect",sub="a=0.04, b=0.08")
ggsave(file="N vs P - large FP effect.pdf",i_j, height=11,width=8)
