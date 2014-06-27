#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 6/27/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output18.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)

# If I have multiple simulations @ the same parameter values, 
# I may want to get summary values - e.g., means, variances - & plot those instead
# use package plyr 

# Y = average average FP cover for all years (except for first three)
FP_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP))) 
FP_plot <- FP_plot + scale_colour_grey()
FP_plot <- FP_plot + geom_point(position="jitter",size=3)
FP_plot <- FP_plot + facet_grid(lightlimitation_FP ~ lightlimitation)
FP_plot <- FP_plot + ylim(0,100)
FP_plot <- FP_plot + xlab("Total N (mg/L)")
FP_plot <- FP_plot + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot <- FP_plot + labs(colour=expression(paste("shadingbyFP")))
FP_plot <- FP_plot + theme_bw(base_size=18)
FP_plot
ggsave(file="output18 - FP&SAV - diff_light_effects - constant NtoP - FP.pdf",FP_plot, height=8,width=11)

SAV_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP))) 
SAV_plot <- SAV_plot + scale_colour_grey()
SAV_plot <- SAV_plot + geom_point(position="jitter",size=3)
SAV_plot <- SAV_plot + facet_grid(lightlimitation_FP ~ lightlimitation)
SAV_plot <- SAV_plot + ylim(0,100)
SAV_plot <- SAV_plot + xlab("Total N (mg/L)")
SAV_plot <- SAV_plot + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot <- SAV_plot + labs(colour=expression(paste("shadingbyFP")))
SAV_plot <- SAV_plot + theme_bw(base_size=18)
SAV_plot
ggsave(file="output18 - FP&SAV - diff_light_effects - constant NtoP - SAV.pdf",SAV_plot, height=8,width=11)


combined_plot <- grid.arrange(FP_plot, SAV_plot,ncol=1, main=NULL,sub=NULL)
combined_plot <- arrangeGrob(FP_plot, SAV_plot,ncol=1, main=NULL,sub=NULL)
ggsave(file="output17 - FP&SAV - with_wind - with_move - constant NtoP.pdf",combined_plot, height=11,width=8)
