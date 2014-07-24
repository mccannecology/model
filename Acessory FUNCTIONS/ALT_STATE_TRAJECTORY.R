#########################################################
# Plot trajectory of FP and SAV growth                  # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 7/24/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output27.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)

data2 <- read.csv("output28.csv") # imports parameter  values for all simulations 
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

data_N7 <- subset(data2, data$TOTALN==4)
data_N8 <- subset(data2, data$TOTALN==5)
data_N9 <- subset(data2, data$TOTALN==6)

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
plot <- ggplot(data_N4_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot <- plot + geom_point() 
plot <- plot + geom_line(arrow=arrow())
plot <- plot + xlab("% SAV cover")
plot <- plot + ylab("% FP cover")
plot <- plot + ylim(0,100)
plot <- plot + xlim(0,100)
plot <- plot + ggtitle("Total N = 4 mg/L")
plot <- plot + theme_classic(base_size=18)
plot <- plot + theme(legend.position="none")
plot

##############
# Plot it    #
# TotalN = 5 #
##############
plot <- ggplot(data_N5_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot <- plot + geom_point() 
plot <- plot + geom_line(arrow=arrow())
plot <- plot + xlab("% SAV cover")
plot <- plot + ylab("% FP cover")
plot <- plot + ylim(0,100)
plot <- plot + xlim(0,100)
plot <- plot + ggtitle("Total N = 5 mg/L")
plot <- plot + theme_classic(base_size=18)
plot <- plot + theme(legend.position="none")
plot

##############
# Plot it    #
# TotalN = 6 #
##############
plot <- ggplot(data_N6_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot <- plot + geom_point() 
plot <- plot + geom_line(arrow=arrow(),alpha=0.4)
plot <- plot + xlab("% SAV cover")
plot <- plot + ylab("% FP cover")
plot <- plot + ylim(0,100)
plot <- plot + xlim(0,100)
plot <- plot + ggtitle("Total N = 6 mg/L")
plot <- plot + theme_classic(base_size=18)
plot <- plot + theme(legend.position="none")
plot

##############
# Plot it    #
# TotalN = 7 #
##############
plot <- ggplot(data_N7_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot <- plot + geom_point() 
plot <- plot + geom_line(arrow=arrow())
plot <- plot + xlab("% SAV cover")
plot <- plot + ylab("% FP cover")
plot <- plot + ylim(0,100)
plot <- plot + xlim(0,100)
plot <- plot + ggtitle("Total N = 7 mg/L")
plot <- plot + theme_classic(base_size=18)
plot <- plot + theme(legend.position="none")
plot

##############
# Plot it    #
# TotalN = 8 #
##############
plot <- ggplot(data_N8_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot <- plot + geom_point() 
plot <- plot + geom_line(arrow=arrow())
plot <- plot + xlab("% SAV cover")
plot <- plot + ylab("% FP cover")
plot <- plot + ylim(0,100)
plot <- plot + xlim(0,100)
plot <- plot + ggtitle("Total N = 8 mg/L")
plot <- plot + theme_classic(base_size=18)
plot <- plot + theme(legend.position="none")
plot

##############
# Plot it    #
# TotalN = 9 #
##############
plot <- ggplot(data_N9_02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot <- plot + geom_point() 
plot <- plot + geom_line(arrow=arrow(),alpha=0.4)
plot <- plot + xlab("% SAV cover")
plot <- plot + ylab("% FP cover")
plot <- plot + ylim(0,100)
plot <- plot + xlim(0,100)
plot <- plot + ggtitle("Total N = 9 mg/L")
plot <- plot + theme_classic(base_size=18)
plot <- plot + theme(legend.position="none")
plot
