#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 7/14/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output22.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)


#########################
# Y = average average FP cover for all years (except for first three)
FP_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover,
                            colour=as.factor(shadingbyFP),
                            shape=as.factor(shadingbyFP))) 
FP_plot <- FP_plot + scale_colour_grey()
FP_plot <- FP_plot + geom_point(position="jitter",size=3)
FP_plot <- FP_plot + facet_grid(lightlimitationSAV ~ lightlimitation01)
FP_plot <- FP_plot + ylim(0,101)
FP_plot <- FP_plot + xlab("Total N (mg/L)")
FP_plot <- FP_plot + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot <- FP_plot + labs(colour=expression(paste("shadingbyFP")))
FP_plot <- FP_plot + labs(shape=expression(paste("shadingbyFP")))
FP_plot <- FP_plot + theme_bw(base_size=18)
FP_plot
ggsave(file="output22 - FP&SAV - lots of changes - FP.pdf",FP_plot, height=8,width=11)

SAV_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover,
                             colour=as.factor(shadingbyFP),
                             shape=as.factor(shadingbyFP))) 
SAV_plot <- SAV_plot + scale_colour_grey()
SAV_plot <- SAV_plot + geom_point(position="jitter",size=3)
SAV_plot <- SAV_plot + facet_grid(lightlimitationSAV ~ lightlimitation01)
SAV_plot <- SAV_plot + ylim(0,101)
SAV_plot <- SAV_plot + xlab("Total N (mg/L)")
SAV_plot <- SAV_plot + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot <- SAV_plot + labs(colour=expression(paste("shadingbyFP")))
SAV_plot <- SAV_plot + labs(shape=expression(paste("shadingbyFP")))
SAV_plot <- SAV_plot + theme_bw(base_size=18)
SAV_plot
ggsave(file="output22 - FP&SAV - lots of changes - SAV.pdf",SAV_plot, height=8,width=11)




########################################
########################################
# initial conditions were 1:1 or 50:50 #
########################################
########################################
data_2 <- subset(data, data$initial_perc_SAV_cover==1 & data$initial_perc_FP_cover==1 |
                   data$initial_perc_SAV_cover==50 & data$initial_perc_FP_cover==50)
head(data_2)
nrow(data_2)


#########################
# Y = average average FP cover for all years (except for first three)
FP_plot_2 <- ggplot(data_2, aes(x=TOTALN,y=avg_avg_FPcover,
                            colour=as.factor(shadingbyFP),
                            shape=as.factor(shadingbyFP))) 
FP_plot_2 <- FP_plot_2 + scale_colour_grey()
FP_plot_2 <- FP_plot_2 + geom_point(position="jitter",size=3)
FP_plot_2 <- FP_plot_2 + facet_grid(lightlimitationSAV ~ lightlimitation01)
FP_plot_2 <- FP_plot_2 + ylim(0,101)
FP_plot_2 <- FP_plot_2 + xlab("Total N (mg/L)")
FP_plot_2 <- FP_plot_2 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot_2 <- FP_plot_2 + labs(colour=expression(paste("shadingbyFP")))
FP_plot_2 <- FP_plot_2 + labs(shape=expression(paste("shadingbyFP")))
FP_plot_2 <- FP_plot_2 + theme_bw(base_size=18)
FP_plot_2
ggsave(file="output22 - FP&SAV - lots of changes - FP - 2.pdf",FP_plot_2, height=8,width=11)

SAV_plot_2 <- ggplot(data_2, aes(x=TOTALN,y=avg_avg_SAVcover,
                             colour=as.factor(shadingbyFP),
                             shape=as.factor(shadingbyFP))) 
SAV_plot_2 <- SAV_plot_2 + scale_colour_grey()
SAV_plot_2 <- SAV_plot_2 + geom_point(position="jitter",size=3)
SAV_plot_2 <- SAV_plot_2 + facet_grid(lightlimitationSAV ~ lightlimitation01)
#SAV_plot_2 <- SAV_plot_2 + ylim(0,101)
SAV_plot_2 <- SAV_plot_2 + xlab("Total N (mg/L)")
SAV_plot_2 <- SAV_plot_2 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot_2 <- SAV_plot_2 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot_2 <- SAV_plot_2 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot_2 <- SAV_plot_2 + theme_bw(base_size=18)
SAV_plot_2
ggsave(file="output22 - FP&SAV - lots of changes - SAV - 2.pdf",SAV_plot_2, height=8,width=11)
