#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
# Updated: 7/14/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output24.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)


#########################
# Y = average average FP cover for all years (except for first three)
FP_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot <- FP_plot + geom_point(position="jitter",size=2)
FP_plot <- FP_plot + facet_grid(initial_perc_SAV_cover ~ initial_perc_FP_cover)
FP_plot <- FP_plot + ylim(0,101)
FP_plot <- FP_plot + xlab("Total N (mg/L)")
FP_plot <- FP_plot + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot <- FP_plot + theme_bw(base_size=18)
FP_plot <- FP_plot + ggtitle("Floating plants")
FP_plot
ggsave(file="output24 - FP&SAV - highshading - lowinitial - FP.pdf",FP_plot, height=11,width=8)

FP_plot_2 <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot_2 <- FP_plot_2 + geom_point(position="jitter",size=2)
FP_plot_2 <- FP_plot_2 + ylim(0,101)
FP_plot_2 <- FP_plot_2 + xlab("Total N (mg/L)")
FP_plot_2 <- FP_plot_2 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot_2 <- FP_plot_2 + theme_bw(base_size=18)
FP_plot_2 <- FP_plot_2 + ggtitle("Floating plants")
FP_plot_2
ggsave(file="output24 - FP&SAV - highshading - lowinitial - FP - 2.pdf",FP_plot_2, height=8,width=8)


SAV_plot <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover)) 
SAV_plot <- SAV_plot + geom_point(position="jitter",size=2)
SAV_plot <- SAV_plot + facet_grid(initial_perc_SAV_cover ~ initial_perc_FP_cover)
SAV_plot <- SAV_plot + ylim(0,101)
SAV_plot <- SAV_plot + xlab("Total N (mg/L)")
SAV_plot <- SAV_plot + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot <- SAV_plot + theme_bw(base_size=18)
SAV_plot <- SAV_plot + ggtitle("Submerged plants")
SAV_plot
ggsave(file="output24 - FP&SAV - highshading - lowinitial - SAV.pdf",SAV_plot, height=11,width=8)


SAV_plot_2 <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover)) 
SAV_plot_2 <- SAV_plot_2 + geom_point(position="jitter",size=2)
SAV_plot_2 <- SAV_plot_2 + ylim(0,101)
SAV_plot_2 <- SAV_plot_2 + xlab("Total N (mg/L)")
SAV_plot_2 <- SAV_plot_2 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot_2 <- SAV_plot_2 + theme_bw(base_size=18)
SAV_plot_2 <- SAV_plot_2 + ggtitle("Submerged plants")
SAV_plot_2
ggsave(file="output24 - FP&SAV - highshading - lowinitial - SAV - 2.pdf",SAV_plot_2, height=8,width=8)

