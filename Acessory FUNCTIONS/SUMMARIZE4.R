#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 6/19/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output14.csv") # imports parameter  values for all simulations 
head(data)

# If I have multiple simulations @ the same parameter values, 
# I may want to get summary values - e.g., means, variances - & plot those instead
# use package plyr 

# Y = average average FP cover for all years (except for first three)
FP_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover,colour=wind_avg)) 
FP_plot <- FP_plot + geom_point(position="jitter")
FP_plot <- FP_plot + facet_grid(. ~ wind_direction)
FP_plot <- FP_plot + xlab("Total N (mg/L)")
FP_plot <- FP_plot + ylab(expression(paste("Annual avg. FP cover\n excl. 1st 3 yr")))
FP_plot <- FP_plot + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plot <- FP_plot + theme_bw(base_size=18)
FP_plot

SAV_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover,colour=wind_avg)) 
SAV_plot <- SAV_plot + geom_point(position="jitter")
SAV_plot <- SAV_plot + facet_grid(. ~ wind_direction)
SAV_plot <- SAV_plot + xlab("Total N (mg/L)")
SAV_plot <- SAV_plot + ylab(expression(paste("Annual avg. SAV cover\n excl. 1st 3 yr")))
SAV_plot <- SAV_plot + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plot <- SAV_plot + theme_bw(base_size=18)
SAV_plot

combined_plot <- grid.arrange(FP_plot, SAV_plot,ncol=1, main=NULL,sub=NULL)
combined_plot <- arrangeGrob(FP_plot, SAV_plot,ncol=1, main=NULL,sub=NULL)
ggsave(file="output14 - FP&SAV - with  wind - constant NtoP.pdf",combined_plot, height=12,width=12)

