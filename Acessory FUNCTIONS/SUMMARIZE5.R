#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 6/20/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output15.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)

# If I have multiple simulations @ the same parameter values, 
# I may want to get summary values - e.g., means, variances - & plot those instead
# use package plyr 

data_withMOVE <- subset(data,data$MOVE=="YES")
data_withoutMOVE <- subset(data,data$MOVE=="NO")

nrow(data_withMOVE)
nrow(data_withoutMOVE)


###############
# With MOVE() #
###############
# Y = average average FP cover for all years (except for first three)
FP_plot_MOVE <- ggplot(data_withMOVE, aes(x=TOTALN,y=avg_avg_FPcover,shape=as.factor(wind_avg))) 
FP_plot_MOVE <- FP_plot_MOVE + geom_point(position="jitter")
FP_plot_MOVE <- FP_plot_MOVE + scale_shape_manual(values=c(0,7,15))
FP_plot_MOVE <- FP_plot_MOVE + facet_grid(. ~ wind_direction)
FP_plot_MOVE <- FP_plot_MOVE + ylim(0,100)
FP_plot_MOVE <- FP_plot_MOVE + xlab("Total N (mg/L)")
FP_plot_MOVE <- FP_plot_MOVE + ylab(expression(paste("Annual avg. FP cover\n excl. 1st 3 yr")))
FP_plot_MOVE <- FP_plot_MOVE + labs(shape=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plot_MOVE <- FP_plot_MOVE + theme_bw(base_size=18)
FP_plot_MOVE

SAV_plot_MOVE <- ggplot(data_withMOVE, aes(x=TOTALN,y=avg_avg_SAVcover,shape=as.factor(wind_avg)))
SAV_plot_MOVE <- SAV_plot_MOVE + geom_point(position="jitter")
SAV_plot_MOVE <- SAV_plot_MOVE + scale_shape_manual(values=c(0,7,15))
SAV_plot_MOVE <- SAV_plot_MOVE + facet_grid(. ~ wind_direction)
SAV_plot_MOVE <- SAV_plot_MOVE + ylim(0,100)
SAV_plot_MOVE <- SAV_plot_MOVE + xlab("Total N (mg/L)")
SAV_plot_MOVE <- SAV_plot_MOVE + ylab(expression(paste("Annual avg. SAV cover\n excl. 1st 3 yr")))
SAV_plot_MOVE <- SAV_plot_MOVE + labs(shape=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plot_MOVE <- SAV_plot_MOVE + theme_bw(base_size=18)
SAV_plot_MOVE

combined_plot_MOVE <- grid.arrange(FP_plot_MOVE, SAV_plot_MOVE,ncol=1, main=NULL,sub=NULL)
combined_plot_MOVE <- arrangeGrob(FP_plot_MOVE, SAV_plot_MOVE,ncol=1, main="with_MOVE",sub=NULL)
ggsave(file="output15 - FP&SAV - with_wind - with_MOVE - constant NtoP.pdf",combined_plot_MOVE, height=12,width=12)


##################
# Without MOVE() #
##################
# Y = average average FP cover for all years (except for first three)
FP_plotnoMOVE <- ggplot(data_withoutMOVE, aes(x=TOTALN,y=avg_avg_FPcover,shape=as.factor(wind_avg))) 
FP_plotnoMOVE <- FP_plotnoMOVE + geom_point(position="jitter")
FP_plotnoMOVE <- FP_plotnoMOVE + scale_shape_manual(values=c(0,7,15))
FP_plotnoMOVE <- FP_plotnoMOVE + facet_grid(. ~ wind_direction)
FP_plotnoMOVE <- FP_plotnoMOVE + ylim(0,100)
FP_plotnoMOVE <- FP_plotnoMOVE + xlab("Total N (mg/L)")
FP_plotnoMOVE <- FP_plotnoMOVE + ylab(expression(paste("Annual avg. FP cover\n excl. 1st 3 yr")))
FP_plotnoMOVE <- FP_plotnoMOVE + labs(shape=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plotnoMOVE <- FP_plotnoMOVE + theme_bw(base_size=18)
FP_plotnoMOVE

SAV_plotnoMOVE <- ggplot(data_withoutMOVE, aes(x=TOTALN,y=avg_avg_SAVcover,shape=as.factor(wind_avg)))
SAV_plotnoMOVE <- SAV_plotnoMOVE + geom_point(position="jitter")
SAV_plotnoMOVE <- SAV_plotnoMOVE + scale_shape_manual(values=c(0,7,15))
SAV_plotnoMOVE <- SAV_plotnoMOVE + facet_grid(. ~ wind_direction)
SAV_plotnoMOVE <- SAV_plotnoMOVE + ylim(0,100)
SAV_plotnoMOVE <- SAV_plotnoMOVE + xlab("Total N (mg/L)")
SAV_plotnoMOVE <- SAV_plotnoMOVE + ylab(expression(paste("Annual avg. SAV cover\n excl. 1st 3 yr")))
SAV_plotnoMOVE <- SAV_plotnoMOVE + labs(shape=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plotnoMOVE <- SAV_plotnoMOVE + theme_bw(base_size=18)
SAV_plotnoMOVE

combined_plot_noMOVE <- grid.arrange(FP_plotnoMOVE, SAV_plotnoMOVE,ncol=1, main=NULL,sub=NULL)
combined_plot_noMOVE <- arrangeGrob(FP_plotnoMOVE, SAV_plotnoMOVE,ncol=1, main="without_MOVE",sub=NULL)
ggsave(file="output15 - FP&SAV - with_wind - without_MOVE - constant NtoP.pdf",combined_plot_noMOVE, height=12,width=12)

