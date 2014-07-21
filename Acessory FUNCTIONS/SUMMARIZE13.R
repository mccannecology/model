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

#########################
# amnt_colonize_SAV = 1 #
# amnt_colonize_FP = 1  #
#########################
data_1_1 <- subset(data, data$amnt_colonize_SAV ==1 & data$amnt_colonize_FP ==1)

# Y = average average FP cover for all years (except for first three)
FP_plot <- ggplot(data_1_1, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(neigh_thresh_FP),shape=as.factor(focal_thresh_FP))) 
FP_plot <- FP_plot + scale_colour_grey()
FP_plot <- FP_plot + geom_point(position="jitter",size=3)
FP_plot <- FP_plot + facet_grid(neigh_thresh_SAV ~ focal_thresh_SAV)
FP_plot <- FP_plot + ylim(0,100)
FP_plot <- FP_plot + xlab("Total N (mg/L)")
FP_plot <- FP_plot + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot <- FP_plot + labs(colour=expression(paste("neigh_thresh_FP")))
FP_plot <- FP_plot + labs(shape=expression(paste("focal_thresh_FP")))
FP_plot <- FP_plot + theme_bw(base_size=18)
FP_plot <- FP_plot + ggtitle("amnt_colonize (SAV,FP): 1,1")
FP_plot
ggsave(file="output25 - FP&SAV - diff_move_pars - amnt_colonize_1_1 - FP.pdf",FP_plot, height=8,width=11)

SAV_plot <- ggplot(data_1_1, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(neigh_thresh_FP),shape=as.factor(focal_thresh_FP))) 
SAV_plot <- SAV_plot + scale_colour_grey()
SAV_plot <- SAV_plot + geom_point(position="jitter",size=3)
SAV_plot <- SAV_plot + facet_grid(neigh_thresh_SAV ~ focal_thresh_SAV)
SAV_plot <- SAV_plot + ylim(0,100)
SAV_plot <- SAV_plot + xlab("Total N (mg/L)")
SAV_plot <- SAV_plot + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot <- SAV_plot + labs(colour=expression(paste("neigh_thresh_FP")))
SAV_plot <- SAV_plot + labs(shape=expression(paste("focal_thresh_FP")))
SAV_plot <- SAV_plot + theme_bw(base_size=18)
SAV_plot <- SAV_plot + ggtitle("amnt_colonize (SAV,FP): 1,1")
SAV_plot
ggsave(file="output25 - FP&SAV - diff_move_pars - amnt_colonize_1_1 - SAV.pdf",SAV_plot, height=8,width=11)
