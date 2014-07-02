#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 6/27/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output19.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)

############
# small    #
# 400 sq.m #
############
# Y = average average FP cover for all years (except for first three)
FP_plot_small <- ggplot(subset(data,data$area==400), aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(wind_avg))) 
FP_plot_small <- FP_plot_small + scale_colour_grey()
FP_plot_small <- FP_plot_small + geom_point(position="jitter",size=3)
FP_plot_small <- FP_plot_small + facet_grid(wind_direction ~ HtoW)
FP_plot_small <- FP_plot_small + ylim(0,100)
FP_plot_small <- FP_plot_small + xlab("Total N (mg/L)")
FP_plot_small <- FP_plot_small + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot_small <- FP_plot_small + labs(colour=expression(paste("wind_avg")))
FP_plot_small <- FP_plot_small + theme_bw(base_size=18)
FP_plot_small <- FP_plot_small + ggtitle("Area: 400 sq. m")
FP_plot_small
ggsave(file="output19 - FP&SAV - shape_size - small - FP.pdf",FP_plot_small, height=8,width=11)

# Y = average average SAV cover for all years (except for first three)
SAV_plot_small <- ggplot(subset(data,data$area==400), aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(wind_avg))) 
SAV_plot_small <- SAV_plot_small + scale_colour_grey()
SAV_plot_small <- SAV_plot_small + geom_point(position="jitter",size=3)
SAV_plot_small <- SAV_plot_small + facet_grid(wind_direction ~ HtoW)
SAV_plot_small <- SAV_plot_small + ylim(0,100)
SAV_plot_small <- SAV_plot_small + xlab("Total N (mg/L)")
SAV_plot_small <- SAV_plot_small + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot_small <- SAV_plot_small + labs(colour=expression(paste("wind_avg")))
SAV_plot_small <- SAV_plot_small + theme_bw(base_size=18)
SAV_plot_small <- SAV_plot_small + ggtitle("Area: 400 sq. m")
SAV_plot_small
ggsave(file="output19 - FP&SAV - shape_size - small - SAV.pdf",SAV_plot_small, height=8,width=11)

#############
# medium    #
# 1600 sq.m #
#############
# Y = average average FP cover for all years (except for first three)
FP_plot_medium <- ggplot(subset(data,data$area==1600), aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(wind_avg))) 
FP_plot_medium <- FP_plot_medium + scale_colour_grey()
FP_plot_medium <- FP_plot_medium + geom_point(position="jitter",size=3)
FP_plot_medium <- FP_plot_medium + facet_grid(wind_direction ~ HtoW)
FP_plot_medium <- FP_plot_medium + ylim(0,100)
FP_plot_medium <- FP_plot_medium + xlab("Total N (mg/L)")
FP_plot_medium <- FP_plot_medium + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot_medium <- FP_plot_medium + labs(colour=expression(paste("wind_avg")))
FP_plot_medium <- FP_plot_medium + theme_bw(base_size=18)
FP_plot_medium <- FP_plot_medium + ggtitle("Area: 1600 sq. m")
FP_plot_medium
ggsave(file="output19 - FP&SAV - shape_size - medium - FP.pdf",FP_plot_medium, height=8,width=11)

# Y = average average SAV cover for all years (except for first three)
SAV_plot_medium <- ggplot(subset(data,data$area==1600), aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(wind_avg))) 
SAV_plot_medium <- SAV_plot_medium + scale_colour_grey()
SAV_plot_medium <- SAV_plot_medium + geom_point(position="jitter",size=3)
SAV_plot_medium <- SAV_plot_medium + facet_grid(wind_direction ~ HtoW)
SAV_plot_medium <- SAV_plot_medium + ylim(0,100)
SAV_plot_medium <- SAV_plot_medium + xlab("Total N (mg/L)")
SAV_plot_medium <- SAV_plot_medium + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot_medium <- SAV_plot_medium + labs(colour=expression(paste("wind_avg")))
SAV_plot_medium <- SAV_plot_medium + theme_bw(base_size=18)
SAV_plot_medium <- SAV_plot_medium + ggtitle("Area: 1600 sq. m")
SAV_plot_medium
ggsave(file="output19 - FP&SAV - shape_size - medium - SAV.pdf",SAV_plot_medium, height=8,width=11)

#############
# large     #
# 3600 sq.m #
#############
# Y = average average FP cover for all years (except for first three)
FP_plot_large <- ggplot(subset(data,data$area==3600), aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(wind_avg))) 
FP_plot_large <- FP_plot_large + scale_colour_grey()
FP_plot_large <- FP_plot_large + geom_point(position="jitter",size=3)
FP_plot_large <- FP_plot_large + facet_grid(wind_direction ~ HtoW)
FP_plot_large <- FP_plot_large + ylim(0,100)
FP_plot_large <- FP_plot_large + xlab("Total N (mg/L)")
FP_plot_large <- FP_plot_large + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot_large <- FP_plot_large + labs(colour=expression(paste("wind_avg")))
FP_plot_large <- FP_plot_large + theme_bw(base_size=18)
FP_plot_large <- FP_plot_large + ggtitle("Area: 3600 sq. m")
FP_plot_large
ggsave(file="output19 - FP&SAV - shape_size - large - FP.pdf",FP_plot_large, height=8,width=11)

# Y = average average SAV cover for all years (except for first three)
SAV_plot_large <- ggplot(subset(data,data$area==3600), aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(wind_avg))) 
SAV_plot_large <- SAV_plot_large + scale_colour_grey()
SAV_plot_large <- SAV_plot_large + geom_point(position="jitter",size=3)
SAV_plot_large <- SAV_plot_large + facet_grid(wind_direction ~ HtoW)
SAV_plot_large <- SAV_plot_large + ylim(0,100)
SAV_plot_large <- SAV_plot_large + xlab("Total N (mg/L)")
SAV_plot_large <- SAV_plot_large + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot_large <- SAV_plot_large + labs(colour=expression(paste("wind_avg")))
SAV_plot_large <- SAV_plot_large + theme_bw(base_size=18)
SAV_plot_large <- SAV_plot_large + ggtitle("Area: 3600 sq. m")
SAV_plot_large
ggsave(file="output19 - FP&SAV - shape_size - large - SAV.pdf",SAV_plot_large, height=8,width=11)
