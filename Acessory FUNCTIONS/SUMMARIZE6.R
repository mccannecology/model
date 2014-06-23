#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 6/20/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output17.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)

# If I have multiple simulations @ the same parameter values, 
# I may want to get summary values - e.g., means, variances - & plot those instead
# use package plyr 

###############
# With MOVE() #
###############
# Y = average average FP cover for all years (except for first three)
FP_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(wind_avg))) 
FP_plot <- FP_plot + geom_point(position="jitter")
FP_plot <- FP_plot + facet_grid(. ~ wind_direction)
FP_plot <- FP_plot + ylim(0,100)
FP_plot <- FP_plot + xlab("Total N (mg/L)")
FP_plot <- FP_plot + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot <- FP_plot + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plot <- FP_plot + theme_bw(base_size=18)
FP_plot

SAV_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(wind_avg)))
SAV_plot <- SAV_plot + geom_point(position="jitter")
SAV_plot <- SAV_plot + facet_grid(. ~ wind_direction)
SAV_plot <- SAV_plot + ylim(0,100)
SAV_plot <- SAV_plot + xlab("Total N (mg/L)")
SAV_plot <- SAV_plot + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot <- SAV_plot + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plot <- SAV_plot + theme_bw(base_size=18)
SAV_plot

combined_plot <- grid.arrange(FP_plot, SAV_plot,ncol=1, main=NULL,sub=NULL)
combined_plot <- arrangeGrob(FP_plot, SAV_plot,ncol=1, main=NULL,sub=NULL)
ggsave(file="output17 - FP&SAV - with_wind - with_move - constant NtoP.pdf",combined_plot, height=11,width=8)

########################################
# Facet by wind_direction and wind_avg #
########################################

###############
# With MOVE() #
###############
# Y = average average FP cover for all years (except for first three)
FP_plot_2 <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot_2 <- FP_plot_2 + geom_point(alpha=0.4)
FP_plot_2 <- FP_plot_2 + facet_grid(wind_avg ~ wind_direction)
FP_plot_2 <- FP_plot_2 + ylim(0,100)
FP_plot_2 <- FP_plot_2 + xlab("Total N (mg/L)")
FP_plot_2 <- FP_plot_2 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot_2 <- FP_plot_2 + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plot_2 <- FP_plot_2 + theme_bw(base_size=18)
FP_plot_2
ggsave(file="output17 - FP&SAV - with_wind - with_move - constant NtoP - FP only.pdf",FP_plot_2, height=11,width=8)

SAV_plot_2 <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot_2 <- SAV_plot_2 + geom_point(alpha=0.4)
SAV_plot_2 <- SAV_plot_2 + facet_grid(wind_avg ~ wind_direction)
SAV_plot_2 <- SAV_plot_2 + ylim(0,100)
SAV_plot_2 <- SAV_plot_2 + xlab("Total N (mg/L)")
SAV_plot_2 <- SAV_plot_2 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot_2 <- SAV_plot_2 + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plot_2 <- SAV_plot_2 + theme_bw(base_size=18)
SAV_plot_2
ggsave(file="output17 - FP&SAV - with_wind - with_move - constant NtoP - SAV only.pdf",SAV_plot_2, height=11,width=8)


combined_plot_2 <- grid.arrange(FP_plot_2, SAV_plot_2,ncol=1, main=NULL,sub=NULL)
combined_plot_2 <- arrangeGrob(FP_plot_2, SAV_plot_2,ncol=1, main="with",sub=NULL)
ggsave(file="output17 - FP&SAV - with_wind - with - constant NtoP - 2.pdf",combined_plot_2, height=11,width=12)


