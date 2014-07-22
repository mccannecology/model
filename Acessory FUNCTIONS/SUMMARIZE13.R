#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 6/27/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output25.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)

######################
# wind_shape1 = 0.04 #
# wind_shape2 = 4    #
# wind_avg = 0.01    #
# wind_stdev = 0.044 #
######################
data_low_wind <- subset(data, data$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot <- ggplot(data_low_wind, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(neigh_thresh_FP),shape=as.factor(focal_thresh_FP))) 
FP_plot <- FP_plot + scale_colour_grey()
FP_plot <- FP_plot + geom_point(position="jitter",size=3)
FP_plot <- FP_plot + facet_grid(neigh_thresh_SAV ~ focal_thresh_SAV)
FP_plot <- FP_plot + ylim(0,100)
FP_plot <- FP_plot + xlab("Total N (mg/L)")
FP_plot <- FP_plot + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot <- FP_plot + labs(colour=expression(paste("neigh_thresh_FP")))
FP_plot <- FP_plot + labs(shape=expression(paste("focal_thresh_FP")))
FP_plot <- FP_plot + theme_bw(base_size=18)
FP_plot <- FP_plot + ggtitle("low_wind: (0.04,4)shape 0.01avg 0.04stdev")
FP_plot
ggsave(file="output25 - FP&SAV - low_wind - diff_move_pars - amnt_colonize_1_1 - FP.jpg",FP_plot, height=8,width=11)

SAV_plot <- ggplot(data_low_wind, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(neigh_thresh_FP),shape=as.factor(focal_thresh_FP))) 
SAV_plot <- SAV_plot + scale_colour_grey()
SAV_plot <- SAV_plot + geom_point(position="jitter",size=3)
SAV_plot <- SAV_plot + facet_grid(neigh_thresh_SAV ~ focal_thresh_SAV)
SAV_plot <- SAV_plot + ylim(0,100)
SAV_plot <- SAV_plot + xlab("Total N (mg/L)")
SAV_plot <- SAV_plot + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot <- SAV_plot + labs(colour=expression(paste("neigh_thresh_FP")))
SAV_plot <- SAV_plot + labs(shape=expression(paste("focal_thresh_FP")))
SAV_plot <- SAV_plot + theme_bw(base_size=18)
SAV_plot <- SAV_plot + ggtitle("low_wind: (0.04,4)shape 0.01avg 0.04stdev")
SAV_plot
ggsave(file="output25 - FP&SAV - low_wind - diff_move_pars - amnt_colonize_1_1 - SAV.jpg",SAV_plot, height=8,width=11)


######################
# wind_shape1 = 0.04 #
# wind_shape2 = 0.2  #
# wind_avg = 0.167   #
# wind_stdev = 0.335 #
######################
data_high_wind <- subset(data, data$wind_shape2==0.2)

# Y = average average FP cover for all years (except for first three)
FP_plot <- ggplot(data_high_wind, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(neigh_thresh_FP),shape=as.factor(focal_thresh_FP))) 
FP_plot <- FP_plot + scale_colour_grey()
FP_plot <- FP_plot + geom_point(position="jitter",size=3)
FP_plot <- FP_plot + facet_grid(neigh_thresh_SAV ~ focal_thresh_SAV)
FP_plot <- FP_plot + ylim(0,100)
FP_plot <- FP_plot + xlab("Total N (mg/L)")
FP_plot <- FP_plot + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot <- FP_plot + labs(colour=expression(paste("neigh_thresh_FP")))
FP_plot <- FP_plot + labs(shape=expression(paste("focal_thresh_FP")))
FP_plot <- FP_plot + theme_bw(base_size=18)
FP_plot <- FP_plot + ggtitle("high_wind: (0.04,0.2)shape 0.17avg 0.34stdev")
FP_plot
ggsave(file="output25 - FP&SAV - high_wind - diff_move_pars - amnt_colonize_1_1 - FP.jpg",FP_plot, height=8,width=11)

SAV_plot <- ggplot(data_high_wind, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(neigh_thresh_FP),shape=as.factor(focal_thresh_FP))) 
SAV_plot <- SAV_plot + scale_colour_grey()
SAV_plot <- SAV_plot + geom_point(position="jitter",size=3)
SAV_plot <- SAV_plot + facet_grid(neigh_thresh_SAV ~ focal_thresh_SAV)
SAV_plot <- SAV_plot + ylim(0,100)
SAV_plot <- SAV_plot + xlab("Total N (mg/L)")
SAV_plot <- SAV_plot + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot <- SAV_plot + labs(colour=expression(paste("neigh_thresh_FP")))
SAV_plot <- SAV_plot + labs(shape=expression(paste("focal_thresh_FP")))
SAV_plot <- SAV_plot + theme_bw(base_size=18)
SAV_plot <- SAV_plot + ggtitle("high_wind: (0.04,0.2)shape 0.17avg 0.34stdev")
SAV_plot
ggsave(file="output25 - FP&SAV - high_wind - diff_move_pars - amnt_colonize_1_1 - SAV.jpg",SAV_plot, height=8,width=11)

