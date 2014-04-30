#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 4/24/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("input09.csv") # imports parameter  values for all simulations 
head(data)
data_smallFPeffect <- data[1:200,] # change these depending no what factors your simulation was looking at 
data_medFPeffect <- data[201:400,]
data_largeFPeffect <- data[401:600,]

# If I have multiple simulations @ the same parameter values, then I need to get summary means, variances (library dplyr) & plot those instead

###################
# small FP effect #
###################
# Y = average average FP cover for all years (except for first three)
#
# light limitation (SAV on SAV): a = 0.01
# FP shading SAV: b = 0.02
e <- ggplot(data_smallFPeffect, aes(x=TOTALN,y=avg_avg_FPcover,colour=initial_perc_FP_cover)) + geom_point(position="jitter")
e <- e + ylab("% FP cover")
e <- e + labs(colour="Initial % FP")
e <- e + theme_classic(base_size=18)
e
f <- ggplot(data_smallFPeffect, aes(x=TOTALN,y=avg_avg_SAVcover,colour=initial_perc_SAV_cover)) + geom_point(position="jitter")
f <- f + ylab("% SAV cover")
f <- f + labs(colour="Initial % SAV")
f <- f + theme_classic(base_size=18)
f

e_f <- grid.arrange(e,f,ncol=1, main="Small FP effect",sub="a=0.01, b=0.02")
e_f <- arrangeGrob(e,f,ncol=1, main="Small FP effect",sub="a=0.01, b=0.02")
ggsave(file="small FP effect.pdf",e_f, height=11,width=8)

#################
# med FP effect #
#################
# Y = average average FP cover for all years (except for first three)
# light limitation (SAV on SAV): a = 0.02
# FP shading SAV: b = 0.04
g <- ggplot(data_medFPeffect, aes(x=TOTALN,y=avg_avg_FPcover,colour=initial_perc_FP_cover)) + geom_point(position="jitter")
g <- g + ylab("% FP cover")
g <- g + labs(colour="Initial % FP")
g <- g + theme_classic(base_size=18)
g

h <- ggplot(data_medFPeffect, aes(x=TOTALN,y=avg_avg_SAVcover,colour=initial_perc_SAV_cover)) + geom_point(position="jitter")
h <- h + ylab("% SAV cover")
h <- h + labs(colour="Initial % SAV")
h <- h + theme_classic(base_size=18)
h

g_h <- grid.arrange(g,h,ncol=1, main="Medium FP effect",sub="a=0.02, b=0.04")
g_h <- arrangeGrob(g,h,ncol=1, main="Medium FP effect",sub="a=0.01, b=0.02")
ggsave(file="med FP effect.pdf",g_h, height=11,width=8)

###################
# large FP effect #
###################
# Y = average average FP cover for all years (except for first three)
# light limitation (SAV on SAV): a = 0.04
# FP shading SAV: b = 0.08
i <- ggplot(data_largeFPeffect, aes(x=TOTALN,y=avg_avg_FPcover,colour=initial_perc_FP_cover)) + geom_point(position="jitter")
i <- i + ylab("% FP cover")
i <- i + labs(colour="Initial % FP")
i <- i + theme_classic(base_size=18)
i

j <- ggplot(data_largeFPeffect, aes(x=TOTALN,y=avg_avg_SAVcover,colour=initial_perc_SAV_cover)) + geom_point(position="jitter")
j <- j + ylab("% SAV cover")
j <- j + labs(colour="Initial % SAV")
j <- j + theme_classic(base_size=18)
j

i_j <- grid.arrange(i,j,ncol=1,main="Large FP effect",sub="a=0.04, b=0.08")
i_j <- arrangeGrob(i,j,ncol=1, main="Large FP effect",sub="a=0.01, b=0.02")
ggsave(file="large FP effect.pdf",i_j, height=11,width=8)
