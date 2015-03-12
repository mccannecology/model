#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
######################################################### 

# This is an example of spatially explicit nutrients 

library(ggplot2)
library(gridExtra)

# combine results - on the first time after the simulation 
data01 <- read.csv("output_wind_size_and_shapeA.csv")
data02 <- read.csv("output_wind_size_and_shapeB.csv")
data03 <- read.csv("output_wind_size_and_shapeC.csv")
data04 <- read.csv("output_wind_size_and_shapeD.csv")
data05 <- read.csv("output_wind_size_and_shapeE.csv")
data06 <- read.csv("output_wind_size_and_shapeF.csv")
data07 <- read.csv("output_wind_size_and_shapeG.csv")
data08 <- read.csv("output_wind_size_and_shapeH.csv")
data09 <- read.csv("output_wind_size_and_shapeI.csv")
data10 <- read.csv("output_wind_size_and_shapeJ.csv")
data11 <- read.csv("output_wind_size_and_shapeK.csv")
data12 <- read.csv("output_wind_size_and_shapeL.csv")
data13 <- read.csv("output_wind_size_and_shapeM.csv")

data <- rbind(data01,data02,data03,data04,data05,data06,
              data07,data08,data09,data10,data11,data12,data13)

write.csv(data,"output_wind_size_and_shape.csv",row.names=F)

# If the results have already been combined 
data <- read.csv("output_wind_size_and_shape.csv")

head(data)
nrow(data)


rm(data01,data02,data03,data04,data05,data06,
   data07,data08,data09,data10,data11,data12,data13)

###############################################
# Calculate "state_score" for each simulation #
# Range (-70.7, 70.7)                         #
# (-) score: more SAV                         #
# (+) score: more FP                          #
###############################################
# I could also use this (avg cover in the final year)
data$state_score <- (data$FP_end_yr04 - data$SAV_end_yr04) / sqrt(2)

#############################################
# Categorize "state_score" into "state"     #
# Discrete categories                       #
# if > 50/sqrt(2) "FP_state"                #   
# if < -50/sqrt(2) "SAV_state"              #
# if -50/sqrt(2) < state_score < 50/sqrt(2) #
# then "mixed"                              #    
#############################################
data$state[data$state_score > 50/sqrt(2)] <- "FP_state"
data$state[data$state_score < -50/sqrt(2)] <- "SAV_state"
data$state[-50/sqrt(2) < data$state_score & data$state_score < 50/sqrt(2)] <- "mixed"


##########################
# Susbset data           #
# diff. wind scalings    #
# Wind scenario A1       #
# weak scaling w/ size   #
# Wind scenario A2       #
# Strong scaling w/ size #
##########################
data_A1 <- subset(data, data$scenario=="A1")
data_A2 <- subset(data, data$scenario=="A2")

################
# Susbset data #
# Diff. sizes  #
################
data_A1_0x04 <- subset(data_A1, data_A1$size=="small")
data_A1_1 <- subset(data_A1, data_A1$size=="med1")
data_A1_2x25 <- subset(data_A1, data_A1$size=="med2")
data_A1_6x25 <- subset(data_A1, data_A1$size=="large")
data_A1_9 <- subset(data_A1, data_A1$size=="XL")

data_A2_0x04 <- subset(data_A2, data_A2$size=="small")
data_A2_1 <- subset(data_A2, data_A2$size=="med1")
data_A2_2x25 <- subset(data_A2, data_A2$size=="med2")
data_A2_6x25 <- subset(data_A2, data_A2$size=="large")
data_A2_9 <- subset(data_A2, data_A2$size=="XL")

################
# Scenario A1  #
# Size 0.04 ha #
################
SAV_FP_plot <- ggplot(data_A1_0x04, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                    
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A1, 0.04 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A1 - 0x04ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A1  #
# Size 1 ha    #
################
SAV_FP_plot <- ggplot(data_A1_1, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                        
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A1, 1 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A1 - 1ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A1  #
# Size 2.25 ha #
################
SAV_FP_plot <- ggplot(data_A1_2x25, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                        
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A1, 2.25 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A1 - 2x25ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A1  #
# Size 6.25 ha #
################
SAV_FP_plot <- ggplot(data_A1_6x25, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                        
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A1, 6.25 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A1 - 6x25ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A1  #
# Size 9 ha    #
################
SAV_FP_plot <- ggplot(data_A1_9, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                        
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A1, 9 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A1 - 9ha.jpg",SAV_FP_plot, height=8,width=12)





################
# Scenario A2  #
# Size 0.04 ha #
################
SAV_FP_plot <- ggplot(data_A2_0x04, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A2, 0.04 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 0x04ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A2  #
# Size 1 ha    #
################
SAV_FP_plot <- ggplot(data_A2_1, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                        
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A2, 1 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 1ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A2  #
# Size 2.25 ha #
################
SAV_FP_plot <- ggplot(data_A2_2x25, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                        
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A2, 2.25 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 2x25ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A2  #
# Size 6.25 ha #
################
SAV_FP_plot <- ggplot(data_A2_6x25, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                        
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A2, 6.25 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 6x25ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A2  #
# Size 9 ha    #
################
SAV_FP_plot <- ggplot(data_A2_9, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                     
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A2, 9 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 9ha.jpg",SAV_FP_plot, height=8,width=12)

# remove  title
SAV_FP_plot <- SAV_FP_plot + ggtitle(NULL)
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 9ha - no_title.jpg",SAV_FP_plot, height=8,width=12)

##################
# Plots under    #  
# more specific  #
# conditions     #
##################

################
# Scenario A1  #
# Facet: size  #
# Facet: wind  #
# Rectangle    #
################
data_temp <- subset(data_A1, data_A1$shape=="rectangle")

data_temp$size <- factor(data_temp$size, levels=c("small", "med1", "med2", "large", "XL"),
                         labels=c("0.04 ha","1 ha", "2.25 ha", "6.25 ha", "9 ha"))

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ size)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario: A1 (rectangle only)")
SAV_FP_plot

ggsave(file="wind & shape & size - A1 - rectangle.jpg",SAV_FP_plot, height=8,width=12)


################
# Scenario A2  #
# Facet: size  #
# Facet: wind  #
# Rectangle    #
################
data_temp <- subset(data_A2, data_A2$shape=="rectangle")

data_temp$size <- factor(data_temp$size, levels=c("small", "med1", "med2", "large", "XL"),
                         labels=c("0.04 ha","1 ha", "2.25 ha", "6.25 ha", "9 ha"))

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ size)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario: A2 (rectangle only)")
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - rectangle.jpg",SAV_FP_plot, height=8,width=12)

# remove  title
SAV_FP_plot <- SAV_FP_plot + ggtitle(NULL)
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - rectangle - no_title.jpg.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A2  #
# Facet: size  #
# Rectangle    #
# wind all     #
################
data_temp <- subset(data_A2, data_A2$wind_direction=="all" & 
                      data_A2$shape=="rectangle")

data_temp$size <- factor(data_temp$size, levels=c("small", "med1", "med2", "large", "XL"),
                         labels=c("0.04 ha","1 ha", "2.25 ha", "6.25 ha", "9 ha"))

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(. ~ size)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - rectangle - all.jpg",SAV_FP_plot, height=4,width=12)


################
# Scenario A2  #
# Facet: size  #
# Rectangle    #
# wind up      #
################
data_temp <- subset(data_A2, data_A2$wind_direction=="up" & 
                      data_A2$shape=="rectangle")

data_temp$size <- factor(data_temp$size, levels=c("small", "med1", "med2", "large", "XL"),
                         labels=c("0.04 ha","1 ha", "2.25 ha", "6.25 ha", "9 ha"))

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(. ~ size)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - rectangle - up.jpg",SAV_FP_plot, height=4,width=12)


################
# Scenario A1  #
# Facet: shape #
# Face: wind   #
# Size: 9ha    #
################
data_temp <- subset(data_A1, data_A1$size=="XL")

data_temp$shape <- factor(data_temp$shape, levels=c("rectangle","eight","hook","tee","cross"))

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario: A1 (9 ha only)")
SAV_FP_plot

ggsave(file="wind & shape & size - A1 - 9ha.jpg",SAV_FP_plot, height=8,width=12)


################
# Scenario A2  #
# Facet: shape #
# Face: wind   #
# Size: 9ha    #
################
data_temp <- subset(data_A2, data_A2$size=="XL")

data_temp$shape <- factor(data_temp$shape, levels=c("rectangle","eight","hook","tee","cross"))

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(wind_direction ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario: A2 (9 ha only)")
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 9ha.jpg",SAV_FP_plot, height=8,width=12)

################
# Scenario A2  #
# Facet: shape #
# Size: 9ha    #
# wind all     #
################
data_temp <- subset(data_A2, data_A2$wind_direction=="all" & 
                      data_A2$size=="XL")

data_temp$shape <- factor(data_temp$shape, levels=c("rectangle","eight","hook","tee","cross"))

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(. ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 9ha - all.jpg",SAV_FP_plot, height=4,width=12)

################
# Scenario A2  #
# Facet: shape #
# Size: 9ha    #
# wind up     #
################
data_temp <- subset(data_A2, data_A2$wind_direction=="up" & 
                      data_A2$size=="XL")

data_temp$shape <- factor(data_temp$shape, levels=c("rectangle","eight","hook","tee","cross"))

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + facet_grid(. ~ shape)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot

ggsave(file="wind & shape & size - A2 - 9ha - up.jpg",SAV_FP_plot, height=4,width=12)



################
# Scenario A1  #
# Size 0.04 ha #
# Rectangle    #
# wind Up      # 
################
data_temp <- subset(data_A1_0x04, data_A1_0x04$wind_direction=="up" 
                    & data_A1_0x04$shape=="rectangle")

SAV_FP_plot <- ggplot(data_temp, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3,
                                        position=position_jitter(w=0.075))
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
#SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=0,colour="black",linetype="dashed")
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
#SAV_FP_plot <- SAV_FP_plot + ggtitle("Scenario A1, 0.04 ha")
SAV_FP_plot

ggsave(file="wind & shape & size - A1 - 0x04ha - rectangle - up.jpg",SAV_FP_plot, height=6,width=6)
