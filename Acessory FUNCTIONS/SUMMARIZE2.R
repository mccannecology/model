#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 4/24/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output11.csv") # imports parameter  values for all simulations 
head(data)
data_medFPeffect <- data[1:270,]
data_largeFPeffect <- data[271:540,]

# If I have multiple simulations @ the same parameter values, then I need to get summary means, variances (library dplyr) & plot those instead

#################
# med FP effect #
#################
# Y = average average FP cover for all years (except for first three)
# light limitation (SAV on SAV): a = 0.02
# FP shading SAV: b = 0.04
g <- ggplot(data_medFPeffect, aes(x=TOTALN,y=TOTALP,colour=avg_avg_FPcover)) + geom_point(position="jitter")
g <- g + xlab("Total N (mg/L)")
g <- g + ylab("Total P (mg/L)")
g <- g + labs(colour="% FP cover")
g <- g + theme_classic(base_size=18)
g

h <- ggplot(data_medFPeffect, aes(x=TOTALN,y=TOTALP,colour=avg_avg_SAVcover)) + geom_point(position="jitter")
h <- h + xlab("Total N (mg/L)")
h <- h + ylab("Total P (mg/L)")
h <- h + labs(colour="% FP cover")
h <- h + theme_classic(base_size=18)
h

g_h <- grid.arrange(g,h,ncol=1, main="Medium FP effect",sub="a=0.02, b=0.04")
g_h <- arrangeGrob(g,h,ncol=1, main="Medium FP effect",sub="a=0.02, b=0.04")
ggsave(file="med FP effect.pdf",g_h, height=16,width=8)

###################
# large FP effect #
###################
# Y = average average FP cover for all years (except for first three)
# light limitation (SAV on SAV): a = 0.04
# FP shading SAV: b = 0.08
i <- ggplot(data_largeFPeffect, aes(x=TOTALN,y=TOTALP,colour=avg_avg_FPcover)) + geom_point(position="jitter")
i <- i + xlab("Total N (mg/L)")
i <- i + ylab("Total P (mg/L)")
i <- i + labs(colour="% FP cover")
i <- i + theme_classic(base_size=18)
i

j <- ggplot(data_largeFPeffect, aes(x=TOTALN,y=TOTALP,colour=avg_avg_SAVcover)) + geom_point(position="jitter")
j <- j + xlab("Total N (mg/L)")
j <- j + ylab("Total P (mg/L)")
j <- j + labs(colour="% FP cover")
j <- j + theme_classic(base_size=18)
j

i_j <- grid.arrange(i,j,ncol=1,main="Large FP effect",sub="a=0.04, b=0.08")
i_j <- arrangeGrob(i,j,ncol=1, main="Large FP effect",sub="a=0.04, b=0.08")
ggsave(file="large FP effect.pdf",i_j, height=16,width=8)
