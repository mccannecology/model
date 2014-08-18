#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data1 <- read.csv("output30a.csv") # imports parameter  values for all simulations 
data2 <- read.csv("output30b.csv") # imports parameter  values for all simulations 
data3 <- read.csv("output30c.csv") # imports parameter  values for all simulations 
data4 <- read.csv("output30d.csv") # imports parameter  values for all simulations 

data <- rbind(data1,data2,data3,data4)

write.csv(data,file="output30.csv",row.names=FALSE)


head(data)
nrow(data)

######################
# rectangle          #
# small (~400 sq m)  #
######################
data_rectangle_small <- subset(data, data$size=="small" & data$shape=="rectangle")

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_rectangle_small, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid(wind_direction ~ wind_shape2)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_rectangle_small")
FP_plot01

SAV_plot01 <- ggplot(data_rectangle_small, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_rectangle_small.jpg",combined_plot01, height=11,width=8)

######################
# rectangle          #
# large (~1600 sq m) #
######################
data_rectangle_large <- subset(data, data$size=="large" & data$shape=="rectangle")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_rectangle_large, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_rectangle_large")
FP_plot02

SAV_plot02 <- ggplot(data_rectangle_large, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot02 <- SAV_plot02 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_rectangle_large.jpg",combined_plot02, height=11,width=8)

######################
# hook               #
# small (~400 sq m)  #
######################
data_hook_small <- subset(data, data$size=="small" & data$shape=="hook")

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_hook_small, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid(wind_direction ~ wind_shape2)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_hook_small")
FP_plot01

SAV_plot01 <- ggplot(data_hook_small, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_hook_small.jpg",combined_plot01, height=11,width=8)

######################
# hook               #
# large (~1600 sq m) #
######################
data_hook_large <- subset(data, data$size=="large" & data$shape=="hook")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_hook_large, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_hook_large")
FP_plot02

SAV_plot02 <- ggplot(data_hook_large, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot02 <- SAV_plot02 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_hook_large.jpg",combined_plot02, height=11,width=8)



######################
# tee                #
# small (~400 sq m)  #
######################
data_tee_small <- subset(data, data$size=="small" & data$shape=="tee")

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_tee_small, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid(wind_direction ~ wind_shape2)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_tee_small")
FP_plot01

SAV_plot01 <- ggplot(data_tee_small, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_tee_small.jpg",combined_plot01, height=11,width=8)

######################
# tee                #
# large (~1600 sq m) #
######################
data_tee_large <- subset(data, data$size=="large" & data$shape=="tee")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_tee_large, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_tee_large")
FP_plot02

SAV_plot02 <- ggplot(data_tee_large, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot02 <- SAV_plot02 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_tee_large.jpg",combined_plot02, height=11,width=8)


######################
# eight              #
# small (~400 sq m)  #
######################
data_eight_small <- subset(data, data$size=="small" & data$shape=="eight")

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_eight_small, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid(wind_direction ~ wind_shape2)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_eight_small")
FP_plot01

SAV_plot01 <- ggplot(data_eight_small, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_eight_small.jpg",combined_plot01, height=11,width=8)

######################
# eight              #
# large (~1600 sq m) #
######################
data_eight_large <- subset(data, data$size=="large" & data$shape=="eight")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_eight_large, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_eight_large")
FP_plot02

SAV_plot02 <- ggplot(data_eight_large, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot02 <- SAV_plot02 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_eight_large.jpg",combined_plot02, height=11,width=8)

######################
# cross              #
# small (~400 sq m)  #
######################
data_cross_small <- subset(data, data$size=="small" & data$shape=="cross")

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_cross_small, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid(wind_direction ~ wind_shape2)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_cross_small")
FP_plot01

SAV_plot01 <- ggplot(data_cross_small, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_cross_small.jpg",combined_plot01, height=11,width=8)

######################
# cross               #
# large (~1600 sq m) #
######################
data_cross_large <- subset(data, data$size=="large" & data$shape=="cross")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_cross_large, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_cross_large")
FP_plot02

SAV_plot02 <- ggplot(data_cross_large, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot02 <- SAV_plot02 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_cross_large.jpg",combined_plot02, height=11,width=8)





################# only wind_direction=="all" #######################
############### and weak wind (wind_shape2==4) #####################


########################
# rectangle            #
# small (~400 sq m)    #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_rectangle_small_all_4 <- subset(data_rectangle_small, 
                                     data_rectangle_small$wind_direction=="all" & 
                                       data_rectangle_small$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_rectangle_small_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_rectangle_small_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_rectangle_small_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_rectangle_small_all_4.jpg",combined_plot01, height=11,width=8)

########################
# rectangle            #
# large (~1600 sq m)   #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_rectangle_large_all_4 <- subset(data_rectangle_large, 
                                     data_rectangle_large$wind_direction=="all" & 
                                       data_rectangle_large$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_rectangle_large_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_rectangle_large_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_rectangle_large_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_rectangle_large_all_4.jpg",combined_plot01, height=11,width=8)



########################
# hook                 #
# small (~400 sq m)    #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_hook_small_all_4 <- subset(data_hook_small, 
                                     data_hook_small$wind_direction=="all" & 
                                       data_hook_small$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_hook_small_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_hook_small_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_hook_small_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_hook_small_all_4.jpg",combined_plot01, height=11,width=8)

########################
# hook                 #
# large (~1600 sq m)   #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_hook_large_all_4 <- subset(data_hook_large, 
                                     data_hook_large$wind_direction=="all" & 
                                       data_hook_large$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_hook_large_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_hook_large_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_hook_large_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_hook_large_all_4.jpg",combined_plot01, height=11,width=8)



########################
# tee                 #
# small (~400 sq m)    #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_tee_small_all_4 <- subset(data_tee_small, 
                                data_tee_small$wind_direction=="all" & 
                                  data_tee_small$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_tee_small_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_tee_small_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_tee_small_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_tee_small_all_4.jpg",combined_plot01, height=11,width=8)

########################
# tee                  #
# large (~1600 sq m)   #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_tee_large_all_4 <- subset(data_tee_large, 
                                data_tee_large$wind_direction=="all" & 
                                  data_tee_large$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_tee_large_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_tee_large_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_tee_large_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_tee_large_all_4.jpg",combined_plot01, height=11,width=8)


########################
# eight                 #
# small (~400 sq m)    #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_eight_small_all_4 <- subset(data_eight_small, 
                                data_eight_small$wind_direction=="all" & 
                                  data_eight_small$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_eight_small_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_eight_small_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_eight_small_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_eight_small_all_4.jpg",combined_plot01, height=11,width=8)

########################
# eight                 #
# large (~1600 sq m)   #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_eight_large_all_4 <- subset(data_eight_large, 
                                data_eight_large$wind_direction=="all" & 
                                  data_eight_large$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_eight_large_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_eight_large_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_eight_large_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_eight_large_all_4.jpg",combined_plot01, height=11,width=8)




########################
# cross                 #
# small (~400 sq m)    #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_cross_small_all_4 <- subset(data_cross_small, 
                                data_cross_small$wind_direction=="all" & 
                                  data_cross_small$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_cross_small_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_cross_small_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_cross_small_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_cross_small_all_4.jpg",combined_plot01, height=11,width=8)

########################
# cross                 #
# large (~1600 sq m)   #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_cross_large_all_4 <- subset(data_cross_large, 
                                data_cross_large$wind_direction=="all" & 
                                  data_cross_large$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_cross_large_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_cross_large_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_cross_large_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output30 - FP&SAV - data_hook_large_all_4.jpg",combined_plot01, height=11,width=8)



########################
# wind_shape==0.2      #
# wind_direction=="up" #
########################
data_wind_high_direction_up <- subset(data, data$wind_direction=="up" & data$wind_shape2==0.2)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_wind_high_direction_up, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid(size ~ shape)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_wind_high_direction_up")
FP_plot01
ggsave(file="output30 - FP&SAV - FP - data_wind_high_direction_up.jpg",FP_plot01, height=8,width=11)

SAV_plot01 <- ggplot(data_wind_high_direction_up, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(size ~ shape)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01 <- SAV_plot01 + ggtitle("data_wind_high_direction_up")
SAV_plot01

ggsave(file="output30 - FP&SAV - SAV - data_wind_high_direction_up.jpg",SAV_plot01, height=8,width=11)


########################
# wind_shape==0.2      #
# wind_direction=="all #
########################
data_wind_high_direction_all <- subset(data, data$wind_direction=="all" & data$wind_shape2==0.2)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_wind_high_direction_all, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid(size ~ shape)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_wind_high_direction_all")
FP_plot01
ggsave(file="output30 - FP&SAV - FP - data_wind_high_direction_all.jpg",FP_plot01, height=8,width=11)

SAV_plot01 <- ggplot(data_wind_high_direction_all, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(size ~ shape)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01 <- SAV_plot01 + ggtitle("data_wind_high_direction_all")
SAV_plot01

ggsave(file="output30 - FP&SAV - SAV - data_wind_high_direction_all.jpg",SAV_plot01, height=8,width=11)

