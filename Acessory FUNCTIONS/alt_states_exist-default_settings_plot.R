library(ggplot2)
library(gridExtra)

data <- read.csv("output_wind_size_and_shape.csv") # imports parameter  values for all simulations 

#####################
# Plant state plots #
#####################
# calculate state score 
data$state_score <- (data$FP_end_yr04 - data$SAV_end_yr04) / sqrt(2)

# get data - wind scenario A1, 1 ha, rectangle, wind in all directions 
temp_data <- subset(data,
                    data$scenario == "A1" & 
                    data$size == "med1" &
                    data$shape == "rectangle" &
                    data$wind_direction == "all")

SAV_FP_plot <- ggplot(temp_data, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_rect(aes(xmin=2.8,
                                           xmax=6.2,
                                           ymin=min(state_score)-2.5,
                                           ymax=max(state_score)+2.5),
                                       fill='thistle2', alpha=0.1)         
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                    
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=0)
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot

ggsave(file="plant_state_plot - default settings.jpg",SAV_FP_plot, height=8,width=12)




##################################
# Alternative state trajectories #
##################################

############### 
# TOTAL N = 1 #
###############
temp_data <- subset(data,
                    data$TOTALN == 1 &
                    data$scenario == "A1" & 
                    data$size == "med1" &
                    data$shape == "rectangle" &
                    data$wind_direction == "all")

temp_data_reshape <- reshape(temp_data,
                             
                             varying = list(
                               
                               c("initial_perc_SAV_cover", 
                                 "SAV_end_yr01",
                                 "SAV_end_yr02",
                                 "SAV_end_yr03",
                                 "SAV_end_yr04"),
                               
                               c("initial_perc_FP_cover",
                                 "FP_end_yr01",
                                 "FP_end_yr02",
                                 "FP_end_yr03",
                                 "FP_end_yr04")
                             ),
                             
                             v.names = c("SAV_cover","FP_cover"),
                             timevar = "time",
                             times = c(0,1,2,3,4),
                             direction = "long"
)

# re-order data by id and time 
temp_data_reshape <- temp_data_reshape[with(temp_data_reshape, order(id, time)), ]

# plot it 
temp_plot_1 <- ggplot(temp_data_reshape, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
temp_plot_1 <- temp_plot_1 + geom_point() 
temp_plot_1 <- temp_plot_1 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
temp_plot_1 <- temp_plot_1 + xlab("% SAV cover")
temp_plot_1 <- temp_plot_1 + ylab("% FP cover")
temp_plot_1 <- temp_plot_1 + ylim(0,100)
temp_plot_1 <- temp_plot_1 + xlim(0,100)
temp_plot_1 <- temp_plot_1 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
temp_plot_1 <- temp_plot_1 + theme_classic(base_size=16)
temp_plot_1 <- temp_plot_1 + theme(legend.position="none")
temp_plot_1 <- temp_plot_1 + ggtitle("1 mg N/L")
temp_plot_1

############### 
# TOTAL N = 5 #
###############
temp_data <- subset(data,
                    data$TOTALN == 5 &
                      data$scenario == "A1" & 
                      data$size == "med1" &
                      data$shape == "rectangle" &
                      data$wind_direction == "all")

temp_data_reshape <- reshape(temp_data,
                             
                             varying = list(
                               
                               c("initial_perc_SAV_cover", 
                                 "SAV_end_yr01",
                                 "SAV_end_yr02",
                                 "SAV_end_yr03",
                                 "SAV_end_yr04"),
                               
                               c("initial_perc_FP_cover",
                                 "FP_end_yr01",
                                 "FP_end_yr02",
                                 "FP_end_yr03",
                                 "FP_end_yr04")
                             ),
                             
                             v.names = c("SAV_cover","FP_cover"),
                             timevar = "time",
                             times = c(0,1,2,3,4),
                             direction = "long"
)

# re-order data by id and time 
temp_data_reshape <- temp_data_reshape[with(temp_data_reshape, order(id, time)), ]

# plot it 
temp_plot_5 <- ggplot(temp_data_reshape, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
temp_plot_5 <- temp_plot_5 + geom_point() 
temp_plot_5 <- temp_plot_5 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.5)
temp_plot_5 <- temp_plot_5 + xlab("% SAV cover")
temp_plot_5 <- temp_plot_5 + ylab("% FP cover")
temp_plot_5 <- temp_plot_5 + ylim(0,100)
temp_plot_5 <- temp_plot_5 + xlim(0,100)
temp_plot_5 <- temp_plot_5 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
temp_plot_5 <- temp_plot_5 + theme_classic(base_size=16)
temp_plot_5 <- temp_plot_5 + theme(legend.position="none")
temp_plot_5 <- temp_plot_5 + ggtitle("5 mg N/L")
temp_plot_5


############### 
# TOTAL N = 9 #
###############
temp_data <- subset(data,
                    data$TOTALN == 9 &
                      data$scenario == "A1" & 
                      data$size == "med1" &
                      data$shape == "rectangle" &
                      data$wind_direction == "all")

temp_data_reshape <- reshape(temp_data,
                             
                             varying = list(
                               
                               c("initial_perc_SAV_cover", 
                                 "SAV_end_yr01",
                                 "SAV_end_yr02",
                                 "SAV_end_yr03",
                                 "SAV_end_yr04"),
                               
                               c("initial_perc_FP_cover",
                                 "FP_end_yr01",
                                 "FP_end_yr02",
                                 "FP_end_yr03",
                                 "FP_end_yr04")
                             ),
                             
                             v.names = c("SAV_cover","FP_cover"),
                             timevar = "time",
                             times = c(0,1,2,3,4),
                             direction = "long"
)

# re-order data by id and time 
temp_data_reshape <- temp_data_reshape[with(temp_data_reshape, order(id, time)), ]

# plot it 
temp_plot_9 <- ggplot(temp_data_reshape, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
temp_plot_9 <- temp_plot_9 + geom_point() 
temp_plot_9 <- temp_plot_9 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
temp_plot_9 <- temp_plot_9 + xlab("% SAV cover")
temp_plot_9 <- temp_plot_9 + ylab("% FP cover")
temp_plot_9 <- temp_plot_9 + ylim(0,100)
temp_plot_9 <- temp_plot_9 + xlim(0,100)
temp_plot_9 <- temp_plot_9 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
temp_plot_9 <- temp_plot_9 + theme_classic(base_size=16)
temp_plot_9 <- temp_plot_9 + theme(legend.position="none")
temp_plot_9 <- temp_plot_9 + ggtitle("9 mg N/L")
temp_plot_9

#######################
# Add labels to plots #
####################### 

#################
# Combine plots # 
#################
# make a blank space-holder plot 
blankPanel <- grid.rect(gp=gpar(col="white")) 

combo_plot <- arrangeGrob(arrangeGrob(blankPanel,SAV_FP_plot,blankPanel, ncol=3,widths=c(0.25,1,0.25)),
                          arrangeGrob(temp_plot_1,temp_plot_5,temp_plot_9, ncol=3),
                          ncol=1, 
                          heights=c(1.75,1))


ggsave("combo_plot-default_settings.jpg",combo_plot,height=10,width=12)
