#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data01 <- read.csv("output31a.csv")
data02 <- read.csv("output31b.csv")
data03 <- read.csv("output31c.csv")
data<-rbind(data01,data02,data03)

head(data)
nrow(data)

######################
# rectangle          #
# larger (~6400 sq m)#
######################
data_rectangle_larger <- subset(data, data$shape=="rectangle")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_rectangle_larger, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_rectangle_larger")
FP_plot02

SAV_plot02 <- ggplot(data_rectangle_larger, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
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

ggsave(file="output31 - FP&SAV - data_rectangle_larger.jpg",combined_plot02, height=11,width=8)

######################
# hook               #
# larger (~6400 sq m) #
######################
data_hook_larger <- subset(data, data$shape=="hook")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_hook_larger, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_hook_larger")
FP_plot02

SAV_plot02 <- ggplot(data_hook_larger, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
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

ggsave(file="output31 - FP&SAV - data_hook_larger.jpg",combined_plot02, height=11,width=8)

######################
# tee                #
# larger (~6400 sq m) #
######################
data_tee_larger <- subset(data, data$shape=="tee")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_tee_larger, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_tee_larger")
FP_plot02

SAV_plot02 <- ggplot(data_tee_larger, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
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

ggsave(file="output31 - FP&SAV - data_tee_larger.jpg",combined_plot02, height=11,width=8)

######################
# eight              #
# larger (~6400 sq m) #
######################
data_eight_larger <- subset(data, data$shape=="eight")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_eight_larger, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_eight_larger")
FP_plot02

SAV_plot02 <- ggplot(data_eight_larger, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
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

ggsave(file="output31 - FP&SAV - data_eight_larger.jpg",combined_plot02, height=11,width=8)

######################
# cross               #
# larger (~6400 sq m) #
######################
data_cross_larger <- subset(data, data$shape=="cross")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_cross_larger, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot02 <- FP_plot02 + labs(colour=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + labs(shape=expression(paste("shadingbyFP")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02 <- FP_plot02 + ggtitle("data_cross_larger")
FP_plot02

SAV_plot02 <- ggplot(data_cross_larger, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
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

ggsave(file="output31 - FP&SAV - data_cross_larger.jpg",combined_plot02, height=11,width=8)





################# only wind_direction=="all" #######################
############### and weak wind (wind_shape2==4) #####################

########################
# rectangle            #
# larger (~6400 sq m)  #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_rectangle_larger_all_4 <- subset(data_rectangle_larger, 
                                     data_rectangle_larger$wind_direction=="all" & 
                                       data_rectangle_larger$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_rectangle_larger_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_rectangle_larger_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_rectangle_larger_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output31 - FP&SAV - data_rectangle_larger_all_4.jpg",combined_plot01, height=11,width=8)


########################
# hook                 #
# larger (~6400 sq m)  #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_hook_larger_all_4 <- subset(data_hook_larger, 
                                     data_hook_larger$wind_direction=="all" & 
                                       data_hook_larger$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_hook_larger_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_hook_larger_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_hook_larger_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output31 - FP&SAV - data_hook_larger_all_4.jpg",combined_plot01, height=11,width=8)

########################
# tee                  #
# larger (~6400 sq m)  #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_tee_larger_all_4 <- subset(data_tee_larger, 
                                data_tee_larger$wind_direction=="all" & 
                                  data_tee_larger$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_tee_larger_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_tee_larger_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_tee_larger_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output31 - FP&SAV - data_tee_larger_all_4.jpg",combined_plot01, height=11,width=8)

########################
# eight                 #
# larger (~6400 sq m)  #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_eight_larger_all_4 <- subset(data_eight_larger, 
                                data_eight_larger$wind_direction=="all" & 
                                  data_eight_larger$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_eight_larger_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_eight_larger_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_eight_larger_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output31 - FP&SAV - data_eight_larger_all_4.jpg",combined_plot01, height=11,width=8)

########################
# cross                #
# larger (~6400 sq m)  #
# wind_direction=="all"#
# wind_shape2==4       #
########################  
data_cross_larger_all_4 <- subset(data_cross_larger, 
                                data_cross_larger$wind_direction=="all" & 
                                  data_cross_larger$wind_shape2==4)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_cross_larger_all_4, aes(x=TOTALN,y=avg_avg_FPcover))
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shadingbyFP)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_cross_larger_all_4")
FP_plot01

SAV_plot01 <- ggplot(data_cross_larger_all_4, aes(x=TOTALN,y=avg_avg_SAVcover))
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid(wind_direction ~ shadingbyFP)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01

combined_plot01 <- arrangeGrob(FP_plot01, SAV_plot01,ncol=1, main=NULL,sub=NULL)

ggsave(file="output31 - FP&SAV - data_cross_larger_all_4.jpg",combined_plot01, height=11,width=8)







########################
# wind_shape==0.2      #
# wind_direction=="up" #
########################
data_wind_high_direction_up <- subset(data, data$wind_direction=="up" & data$wind_shape2==0.2)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_wind_high_direction_up, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shape)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_wind_high_direction_up")
FP_plot01
ggsave(file="output31 - FP&SAV - FP - data_wind_high_direction_up.jpg",FP_plot01, height=8,width=11)

SAV_plot01 <- ggplot(data_wind_high_direction_up, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid( ~ shape)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01 <- SAV_plot01 + ggtitle("data_wind_high_direction_up")
SAV_plot01
ggsave(file="output31 - FP&SAV - SAV - data_wind_high_direction_up.jpg",SAV_plot01, height=8,width=11)


########################
# wind_shape==0.2      #
# wind_direction=="all #
########################
data_wind_high_direction_all <- subset(data, data$wind_direction=="all" & data$wind_shape2==0.2)

# Y = average average FP cover for all years (except for first three)
FP_plot01 <- ggplot(data_wind_high_direction_all, aes(x=TOTALN,y=avg_avg_FPcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
FP_plot01 <- FP_plot01 + scale_colour_grey()
FP_plot01 <- FP_plot01 + geom_point(position="jitter",size=3)
FP_plot01 <- FP_plot01 + facet_grid( ~ shape)
FP_plot01 <- FP_plot01 + ylim(0,100)
FP_plot01 <- FP_plot01 + xlab("Total N (mg/L)")
FP_plot01 <- FP_plot01 + ylab(expression(paste("Annual avg. FP cover excl. 1st 3 yr")))
FP_plot01 <- FP_plot01 + labs(colour=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + labs(shape=expression(paste("shadingbyFP")))
FP_plot01 <- FP_plot01 + theme_bw(base_size=18)
FP_plot01 <- FP_plot01 + ggtitle("data_wind_high_direction_all")
FP_plot01
ggsave(file="output31 - FP&SAV - FP - data_wind_high_direction_all.jpg",FP_plot01, height=8,width=11)

SAV_plot01 <- ggplot(data_wind_high_direction_all, aes(x=TOTALN,y=avg_avg_SAVcover,colour=as.factor(shadingbyFP),shape=as.factor(shadingbyFP))) 
SAV_plot01 <- SAV_plot01 + scale_colour_grey()
SAV_plot01 <- SAV_plot01 + geom_point(position="jitter",size=3)
SAV_plot01 <- SAV_plot01 + facet_grid( ~ shape)
SAV_plot01 <- SAV_plot01 + ylim(0,100)
SAV_plot01 <- SAV_plot01 + xlab("Total N (mg/L)")
SAV_plot01 <- SAV_plot01 + ylab(expression(paste("Annual avg. SAV cover excl. 1st 3 yr")))
SAV_plot01 <- SAV_plot01 + labs(colour=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + labs(shape=expression(paste("shadingbyFP")))
SAV_plot01 <- SAV_plot01 + theme_bw(base_size=18)
SAV_plot01 <- SAV_plot01 + ggtitle("data_wind_high_direction_all")
SAV_plot01
ggsave(file="output31 - FP&SAV - SAV - data_wind_high_direction_all.jpg",SAV_plot01, height=8,width=11)

