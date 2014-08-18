#########################################################
# Plot trajectory of FP and SAV growth                  # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 7/24/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output30.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)

###################
# Subset          #      
# same parameters #
# same nutrients  #
###################
data_N4 <- subset(data, data$TOTALN==4)
data_N5 <- subset(data, data$TOTALN==5)
data_N6 <- subset(data, data$TOTALN==6)

data_N7 <- subset(data, data$TOTALN==7)
data_N8 <- subset(data, data$TOTALN==8)
data_N9 <- subset(data, data$TOTALN==9)



# For loop through variables to subset data 

for (i in unique(data$TOTALN)){
  for (j in unique(data$wind_shape2)){
    for (k in unique(data$wind_direction)){
      for (l in unique(data$shadingbyFP)){
        temp_data <- subset(data, data$TOTALN==i & data$wind_shape2==j & 
                              data$wind_direction==k & data$shadingbyFP==l)
        
        temp_data_reshape <- reshape(temp_data,
                              
                              varying = list(
                                
                                c("initial_perc_SAV_cover", 
                                  "avg_SAVcover_yr01",
                                  "avg_SAVcover_yr02",
                                  "avg_SAVcover_yr03"),
                                
                                c("initial_perc_FP_cover",
                                  "avg_FPcover_yr01",
                                  "avg_FPcover_yr02",
                                  "avg_FPcover_yr03")
                              ),
                              
                              v.names = c("SAV_cover","FP_cover"),
                              timevar = "time",
                              times = c(0,1,2,3),
                              direction = "long"
        )
        
        # re-order data by id and time 
        temp_data_reshape <- temp_data_reshape[with(temp_data_reshape, order(id, time)), ]
        
        # plot it 
        temp_plot <- ggplot(temp_data_reshape, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
        temp_plot <- temp_plot + geom_point() 
        temp_plot <- temp_plot + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
        temp_plot <- temp_plot + facet_grid(shape ~ size)
        temp_plot <- temp_plot + xlab("% SAV cover")
        temp_plot <- temp_plot + ylab("% FP cover")
        temp_plot <- temp_plot + ylim(0,100)
        temp_plot <- temp_plot + xlim(0,100)
        temp_plot <- temp_plot + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
        temp_plot <- temp_plot + ggtitle(paste("N=",i,",wind_2=",j,",dir=",k,",shade=",l,sep=""))
        temp_plot <- temp_plot + theme_classic(base_size=16)
        temp_plot <- temp_plot + theme(legend.position="none")
        # temp_plot
        
        ggsave(file=paste("alt_state_N=",i,",wind=",j,",dir=",k,",shade=",l,".jpg",sep=""),temp_plot,height=9,width=5)
                
        #print(nrow(temp_data_reshape))
        #print(nrow(temp_data))
      }
    }
  }
}




###################
# Manipulate data #
###################
data_N4_02 <- reshape(data_N4,
                  
                  varying = list(
                     
                    c("initial_perc_SAV_cover", 
                       "avg_SAVcover_yr01",
                       "avg_SAVcover_yr02",
                       "avg_SAVcover_yr03"),
                     
                     c("initial_perc_FP_cover",
                       "avg_FPcover_yr01",
                       "avg_FPcover_yr02",
                       "avg_FPcover_yr03")
                  ),
                  
                  v.names = c("SAV_cover","FP_cover"),
                  timevar = "time",
                  times = c(0,1,2,3),
                  direction = "long"
                  )

data_N5_02 <- reshape(data_N5,
                      
                      varying = list(
                        
                        c("initial_perc_SAV_cover", 
                          "avg_SAVcover_yr01",
                          "avg_SAVcover_yr02",
                          "avg_SAVcover_yr03"),
                        
                        c("initial_perc_FP_cover",
                          "avg_FPcover_yr01",
                          "avg_FPcover_yr02",
                          "avg_FPcover_yr03")
                      ),
                      
                      v.names = c("SAV_cover","FP_cover"),
                      timevar = "time",
                      times = c(0,1,2,3),
                      direction = "long"
)

data_N6_02 <- reshape(data_N6,
                      
                      varying = list(
                        
                        c("initial_perc_SAV_cover", 
                          "avg_SAVcover_yr01",
                          "avg_SAVcover_yr02",
                          "avg_SAVcover_yr03"),
                        
                        c("initial_perc_FP_cover",
                          "avg_FPcover_yr01",
                          "avg_FPcover_yr02",
                          "avg_FPcover_yr03")
                      ),
                      
                      v.names = c("SAV_cover","FP_cover"),
                      timevar = "time",
                      times = c(0,1,2,3),
                      direction = "long"
)

data_N7_02 <- reshape(data_N7,
                      
                      varying = list(
                        
                        c("initial_perc_SAV_cover", 
                          "avg_SAVcover_yr01",
                          "avg_SAVcover_yr02",
                          "avg_SAVcover_yr03"),
                        
                        c("initial_perc_FP_cover",
                          "avg_FPcover_yr01",
                          "avg_FPcover_yr02",
                          "avg_FPcover_yr03")
                      ),
                      
                      v.names = c("SAV_cover","FP_cover"),
                      timevar = "time",
                      times = c(0,1,2,3),
                      direction = "long"
)

data_N8_02 <- reshape(data_N8,
                      
                      varying = list(
                        
                        c("initial_perc_SAV_cover", 
                          "avg_SAVcover_yr01",
                          "avg_SAVcover_yr02",
                          "avg_SAVcover_yr03"),
                        
                        c("initial_perc_FP_cover",
                          "avg_FPcover_yr01",
                          "avg_FPcover_yr02",
                          "avg_FPcover_yr03")
                      ),
                      
                      v.names = c("SAV_cover","FP_cover"),
                      timevar = "time",
                      times = c(0,1,2,3),
                      direction = "long"
)

data_N9_02 <- reshape(data_N9,
                      
                      varying = list(
                        
                        c("initial_perc_SAV_cover", 
                          "avg_SAVcover_yr01",
                          "avg_SAVcover_yr02",
                          "avg_SAVcover_yr03"),
                        
                        c("initial_perc_FP_cover",
                          "avg_FPcover_yr01",
                          "avg_FPcover_yr02",
                          "avg_FPcover_yr03")
                      ),
                      
                      v.names = c("SAV_cover","FP_cover"),
                      timevar = "time",
                      times = c(0,1,2,3),
                      direction = "long"
)

##############
# Plot it    #
# TotalN = 4 #
##############
# re-order data by id and time 
data_N4_02 <- data_N4_02[with(data_N4_02, order(id, time)), ]

plot_N4 <- ggplot(data_N4_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot_N4 <- plot_N4 + geom_point() 
plot_N4 <- plot_N4 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
plot_N4 <- plot_N4 + xlab("% SAV cover")
plot_N4 <- plot_N4 + ylab("% FP cover")
plot_N4 <- plot_N4 + ylim(0,100)
plot_N4 <- plot_N4 + xlim(0,100)
plot_N4 <- plot_N4 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
plot_N4 <- plot_N4 + ggtitle("Total N = 4 mg/L")
plot_N4 <- plot_N4 + theme_classic(base_size=18)
plot_N4 <- plot_N4 + theme(legend.position="none")
plot_N4

ggsave("alt_states_total_N4.jpg",plot_N4,height=8,width=8)


##############
# Plot it    #
# TotalN = 5 #
##############
# re-order data by id and time 
data_N5_02 <- data_N5_02[with(data_N5_02, order(id, time)), ]

plot_N5 <- ggplot(data_N5_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot_N5 <- plot_N5 + geom_point() 
plot_N5 <- plot_N5 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
plot_N5 <- plot_N5 + xlab("% SAV cover")
plot_N5 <- plot_N5 + ylab("% FP cover")
plot_N5 <- plot_N5 + ylim(0,100)
plot_N5 <- plot_N5 + xlim(0,100)
plot_N5 <- plot_N5 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
plot_N5 <- plot_N5 + ggtitle("Total N = 5 mg/L")
plot_N5 <- plot_N5 + theme_classic(base_size=18)
plot_N5 <- plot_N5 + theme(legend.position="none")
plot_N5

ggsave("alt_states_total_N5.jpg",plot_N5,height=8,width=8)

##############
# Plot it    #
# TotalN = 6 #
##############
# re-order data by id and time 
data_N6_02 <- data_N6_02[with(data_N6_02, order(id, time)), ]

plot_N6 <- ggplot(data_N6_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot_N6 <- plot_N6 + geom_point() 
plot_N6 <- plot_N6 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
plot_N6 <- plot_N6 + xlab("% SAV cover")
plot_N6 <- plot_N6 + ylab("% FP cover")
plot_N6 <- plot_N6 + ylim(0,100)
plot_N6 <- plot_N6 + xlim(0,100)
plot_N6 <- plot_N6 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
plot_N6 <- plot_N6 + ggtitle("Total N = 6 mg/L")
plot_N6 <- plot_N6 + theme_classic(base_size=18)
plot_N6 <- plot_N6 + theme(legend.position="none")
plot_N6

ggsave("alt_states_total_N6.jpg",plot_N6,height=8,width=8)


##############
# Plot it    #
# TotalN = 7 #
##############
data_N7_02 <- data_N7_02[with(data_N7_02, order(id, time)), ]

plot_N7 <- ggplot(data_N7_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot_N7 <- plot_N7 + geom_point() 
plot_N7 <- plot_N7 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
plot_N7 <- plot_N7 + xlab("% SAV cover")
plot_N7 <- plot_N7 + ylab("% FP cover")
plot_N7 <- plot_N7 + ylim(0,100)
plot_N7 <- plot_N7 + xlim(0,100)
plot_N7 <- plot_N7 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
plot_N7 <- plot_N7 + ggtitle("Total N = 7 mg/L")
plot_N7 <- plot_N7 + theme_classic(base_size=18)
plot_N7 <- plot_N7 + theme(legend.position="none")
plot_N7

ggsave("alt_states_total_N7.jpg",plot_N7,height=8,width=8)


##############
# Plot it    #
# TotalN = 8 #
##############
data_N8_02 <- data_N8_02[with(data_N8_02, order(id, time)), ]

plot_N8 <- ggplot(data_N8_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot_N8 <- plot_N8 + geom_point() 
plot_N8 <- plot_N8 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
plot_N8 <- plot_N8 + xlab("% SAV cover")
plot_N8 <- plot_N8 + ylab("% FP cover")
plot_N8 <- plot_N8 + ylim(0,100)
plot_N8 <- plot_N8 + xlim(0,100)
plot_N8 <- plot_N8 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
plot_N8 <- plot_N8 + ggtitle("Total N = 8 mg/L")
plot_N8 <- plot_N8 + theme_classic(base_size=18)
plot_N8 <- plot_N8 + theme(legend.position="none")
plot_N8

ggsave("alt_states_total_N8.jpg",plot_N8,height=8,width=8)

##############
# Plot it    #
# TotalN = 9 #
##############
data_N9_02 <- data_N9_02[with(data_N9_02, order(id, time)), ]

plot_N9 <- ggplot(data_N9_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot_N9 <- plot_N9 + geom_point() 
plot_N9 <- plot_N9 + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
plot_N9 <- plot_N9 + xlab("% SAV cover")
plot_N9 <- plot_N9 + ylab("% FP cover")
plot_N9 <- plot_N9 + ylim(0,100)
plot_N9 <- plot_N9 + xlim(0,100)
plot_N9 <- plot_N9 + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
plot_N9 <- plot_N9 + ggtitle("Total N = 9 mg/L")
plot_N9 <- plot_N9 + theme_classic(base_size=18)
plot_N9 <- plot_N9 + theme(legend.position="none")
plot_N9

ggsave("alt_states_total_N9.jpg",plot_N9,height=8,width=8)
