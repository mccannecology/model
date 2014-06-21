#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 6/20/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output16.csv") # imports parameter  values for all simulations 
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
FP_plot_MOVE <- ggplot(data_withMOVE, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(wind_avg))) 
FP_plot_MOVE <- FP_plot_MOVE + geom_point(position="jitter")
FP_plot_MOVE <- FP_plot_MOVE + facet_grid(. ~ wind_direction)
FP_plot_MOVE <- FP_plot_MOVE + ylim(0,100)
FP_plot_MOVE <- FP_plot_MOVE + xlab("Total N (mg/L)")
FP_plot_MOVE <- FP_plot_MOVE + ylab(expression(paste("Annual avg. FP cover\n excl. 1st 3 yr")))
FP_plot_MOVE <- FP_plot_MOVE + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plot_MOVE <- FP_plot_MOVE + theme_bw(base_size=18)
FP_plot_MOVE

SAV_plot_MOVE <- ggplot(data_withMOVE, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(wind_avg)))
SAV_plot_MOVE <- SAV_plot_MOVE + geom_point(position="jitter")
SAV_plot_MOVE <- SAV_plot_MOVE + facet_grid(. ~ wind_direction)
SAV_plot_MOVE <- SAV_plot_MOVE + ylim(0,100)
SAV_plot_MOVE <- SAV_plot_MOVE + xlab("Total N (mg/L)")
SAV_plot_MOVE <- SAV_plot_MOVE + ylab(expression(paste("Annual avg. SAV cover\n excl. 1st 3 yr")))
SAV_plot_MOVE <- SAV_plot_MOVE + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plot_MOVE <- SAV_plot_MOVE + theme_bw(base_size=18)
SAV_plot_MOVE

combined_plot_MOVE <- grid.arrange(FP_plot_MOVE, SAV_plot_MOVE,ncol=1, main=NULL,sub=NULL)
combined_plot_MOVE <- arrangeGrob(FP_plot_MOVE, SAV_plot_MOVE,ncol=1, main="with_MOVE",sub=NULL)
ggsave(file="output16 - FP&SAV - with_wind - with_MOVE - constant NtoP.pdf",combined_plot_MOVE, height=12,width=12)


##################
# Without MOVE() #
##################
# Y = average average FP cover for all years (except for first three)
FP_plotnoMOVE <- ggplot(data_withoutMOVE, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(wind_avg))) 
FP_plotnoMOVE <- FP_plotnoMOVE + geom_point(position="jitter")
FP_plotnoMOVE <- FP_plotnoMOVE + facet_grid(. ~ wind_direction)
FP_plotnoMOVE <- FP_plotnoMOVE + ylim(0,100)
FP_plotnoMOVE <- FP_plotnoMOVE + xlab("Total N (mg/L)")
FP_plotnoMOVE <- FP_plotnoMOVE + ylab(expression(paste("Annual avg. FP cover\n excl. 1st 3 yr")))
FP_plotnoMOVE <- FP_plotnoMOVE + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plotnoMOVE <- FP_plotnoMOVE + theme_bw(base_size=18)
FP_plotnoMOVE

SAV_plotnoMOVE <- ggplot(data_withoutMOVE, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(wind_avg)))
SAV_plotnoMOVE <- SAV_plotnoMOVE + geom_point(position="jitter")
SAV_plotnoMOVE <- SAV_plotnoMOVE + scale_shape_manual(values=c(0,7,15))
SAV_plotnoMOVE <- SAV_plotnoMOVE + facet_grid(. ~ wind_direction)
SAV_plotnoMOVE <- SAV_plotnoMOVE + ylim(0,100)
SAV_plotnoMOVE <- SAV_plotnoMOVE + xlab("Total N (mg/L)")
SAV_plotnoMOVE <- SAV_plotnoMOVE + ylab(expression(paste("Annual avg. SAV cover\n excl. 1st 3 yr")))
SAV_plotnoMOVE <- SAV_plotnoMOVE + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plotnoMOVE <- SAV_plotnoMOVE + theme_bw(base_size=18)
SAV_plotnoMOVE

combined_plot_noMOVE <- grid.arrange(FP_plotnoMOVE, SAV_plotnoMOVE,ncol=1, main=NULL,sub=NULL)
combined_plot_noMOVE <- arrangeGrob(FP_plotnoMOVE, SAV_plotnoMOVE,ncol=1, main="without_MOVE",sub=NULL)
ggsave(file="output16 - FP&SAV - with_wind - without_MOVE - constant NtoP.pdf",combined_plot_noMOVE, height=12,width=12)


########################################
# Facet by wind_direction and wind_avg #
########################################

###############
# With MOVE() #
###############
# Y = average average FP cover for all years (except for first three)
FP_plot_MOVE_2 <- ggplot(data_withMOVE, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot_MOVE_2 <- FP_plot_MOVE_2 + geom_point(position="jitter")
FP_plot_MOVE_2 <- FP_plot_MOVE_2 + facet_grid(wind_avg ~ wind_direction)
FP_plot_MOVE_2 <- FP_plot_MOVE_2 + ylim(0,100)
FP_plot_MOVE_2 <- FP_plot_MOVE_2 + xlab("Total N (mg/L)")
FP_plot_MOVE_2 <- FP_plot_MOVE_2 + ylab(expression(paste("Annual avg. FP cover\n excl. 1st 3 yr")))
FP_plot_MOVE_2 <- FP_plot_MOVE_2 + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plot_MOVE_2 <- FP_plot_MOVE_2 + theme_bw(base_size=18)
FP_plot_MOVE_2

SAV_plot_MOVE_2 <- ggplot(data_withMOVE, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot_MOVE_2 <- SAV_plot_MOVE_2 + geom_point(position="jitter")
SAV_plot_MOVE_2 <- SAV_plot_MOVE_2 + facet_grid(wind_avg ~ wind_direction)
SAV_plot_MOVE_2 <- SAV_plot_MOVE_2 + ylim(0,100)
SAV_plot_MOVE_2 <- SAV_plot_MOVE_2 + xlab("Total N (mg/L)")
SAV_plot_MOVE_2 <- SAV_plot_MOVE_2 + ylab(expression(paste("Annual avg. SAV cover\n excl. 1st 3 yr")))
SAV_plot_MOVE_2 <- SAV_plot_MOVE_2 + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plot_MOVE_2 <- SAV_plot_MOVE_2 + theme_bw(base_size=18)
SAV_plot_MOVE_2

combined_plot_MOVE_2 <- grid.arrange(FP_plot_MOVE_2, SAV_plot_MOVE_2,ncol=1, main=NULL,sub=NULL)
combined_plot_MOVE_2 <- arrangeGrob(FP_plot_MOVE_2, SAV_plot_MOVE_2,ncol=1, main="with_MOVE",sub=NULL)
ggsave(file="output16 - FP&SAV - with_wind - with_MOVE - constant NtoP - 2.pdf",combined_plot_MOVE_2, height=12,width=12)


##################
# Without MOVE() #
##################
# Y = average average FP cover for all years (except for first three)
FP_plotnoMOVE_2 <- ggplot(data_withoutMOVE, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plotnoMOVE_2 <- FP_plotnoMOVE_2 + geom_point(position="jitter")
FP_plotnoMOVE_2 <- FP_plotnoMOVE_2 + facet_grid(wind_avg ~ wind_direction)
FP_plotnoMOVE_2 <- FP_plotnoMOVE_2 + ylim(0,100)
FP_plotnoMOVE_2 <- FP_plotnoMOVE_2 + xlab("Total N (mg/L)")
FP_plotnoMOVE_2 <- FP_plotnoMOVE_2 + ylab(expression(paste("Annual avg. FP cover\n excl. 1st 3 yr")))
FP_plotnoMOVE_2 <- FP_plotnoMOVE_2 + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
FP_plotnoMOVE_2 <- FP_plotnoMOVE_2 + theme_bw(base_size=18)
FP_plotnoMOVE_2

SAV_plotnoMOVE_2 <- ggplot(data_withoutMOVE, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plotnoMOVE_2 <- SAV_plotnoMOVE_2 + geom_point(position="jitter")
SAV_plotnoMOVE_2 <- SAV_plotnoMOVE_2 + scale_shape_manual(values=c(0,7,15))
SAV_plotnoMOVE_2 <- SAV_plotnoMOVE_2 + facet_grid(wind_avg ~ wind_direction)
SAV_plotnoMOVE_2 <- SAV_plotnoMOVE_2 + ylim(0,100)
SAV_plotnoMOVE_2 <- SAV_plotnoMOVE_2 + xlab("Total N (mg/L)")
SAV_plotnoMOVE_2 <- SAV_plotnoMOVE_2 + ylab(expression(paste("Annual avg. SAV cover\n excl. 1st 3 yr")))
SAV_plotnoMOVE_2 <- SAV_plotnoMOVE_2 + labs(colour=expression(paste("Avg. prop. biom.\n moved by wind")))
SAV_plotnoMOVE_2 <- SAV_plotnoMOVE_2 + theme_bw(base_size=18)
SAV_plotnoMOVE_2

combined_plot_noMOVE_2 <- grid.arrange(FP_plotnoMOVE_2, SAV_plotnoMOVE_2,ncol=1, main=NULL,sub=NULL)
combined_plot_noMOVE_2 <- arrangeGrob(FP_plotnoMOVE_2, SAV_plotnoMOVE_2,ncol=1, main="without_MOVE",sub=NULL)
ggsave(file="output16 - FP&SAV - with_wind - without_MOVE - constant NtoP - 2.pdf",combined_plot_noMOVE, height=12,width=12)
