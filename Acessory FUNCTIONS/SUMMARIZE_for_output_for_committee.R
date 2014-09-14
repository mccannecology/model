#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
######################################################### 

# This is an example of spatially explicit nutrients 

library(ggplot2)
library(gridExtra)

# combine the two output chunks into one 
data <- read.csv("output_for_committee.csv")

colnames(data)
nrow(data)

###############################################
# Calculate "state_score" for each simulation #
# Range (-70.7, 70.7)                         #
# (-) score: more SAV                         #
# (+) score: more FP                          #
###############################################
# I could also use this (avg cover in the final year)
data$state_score <- (data$avg_avg_FPcover - data$avg_avg_SAVcover) / sqrt(2)

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

###################
# rectangle       #
# small (0.04 ha) #
###################
data_small <- subset(data, data$size=="small")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_small, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Avg. FP cover year 4")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02

SAV_plot02 <- ggplot(data_small, aes(x=TOTALN,y=avg_avg_SAVcover)) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Avg. SAV cover year 4")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output_for_committee - FP&SAV - non-spatial-nutrients - rectangle - 0_04ha.jpg",combined_plot02, height=11,width=8)

##########################
# Re-do the plots above  #
# But with "state_score" #
##########################

###################
# rectangle       #
# small (0.04 ha) #  # try different facetting - see what looks best 
# state_score     #
###################
# Y = average average FP cover for all years (except for first three)
SAV_FP_plot2 <- ggplot(data_small, aes(x=TOTALN,y=state_score))
SAV_FP_plot2 <- SAV_FP_plot2 + scale_colour_grey()
SAV_FP_plot2 <- SAV_FP_plot2 + geom_point(position="jitter",size=3)
SAV_FP_plot2 <- SAV_FP_plot2 + facet_grid(wind_direction ~ wind_shape2)
SAV_FP_plot2 <- SAV_FP_plot2 + xlab("Total N (mg/L)")
SAV_FP_plot2 <- SAV_FP_plot2 + ylab(expression(paste("Plant state score")))
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + theme_bw(base_size=18)
SAV_FP_plot2 <- SAV_FP_plot2 + ggtitle("")
SAV_FP_plot2

ggsave(file="output_for_committee - state_score - non-spatial_nutrients - rectangle- 0_04ha.jpg",SAV_FP_plot2, height=11,width=8)



###################
# rectangle       #
# medium (1 ha) #
###################
data_medium <- subset(data, data$size=="medium")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_medium, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Avg. FP cover year 4")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02

SAV_plot02 <- ggplot(data_medium, aes(x=TOTALN,y=avg_avg_SAVcover)) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Avg. SAV cover year 4")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output_for_committee - FP&SAV - non-spatial-nutrients - rectangle - 1ha.jpg",combined_plot02, height=11,width=8)

##########################
# Re-do the plots above  #
# But with "state_score" #
##########################

###################
# rectangle       #
# medium (1 ha) #  # try different facetting - see what looks best 
# state_score     #
###################
# Y = average average FP cover for all years (except for first three)
SAV_FP_plot2 <- ggplot(data_medium, aes(x=TOTALN,y=state_score))
SAV_FP_plot2 <- SAV_FP_plot2 + scale_colour_grey()
SAV_FP_plot2 <- SAV_FP_plot2 + geom_point(position="jitter",size=3)
SAV_FP_plot2 <- SAV_FP_plot2 + facet_grid(wind_direction ~ wind_shape2)
SAV_FP_plot2 <- SAV_FP_plot2 + xlab("Total N (mg/L)")
SAV_FP_plot2 <- SAV_FP_plot2 + ylab(expression(paste("Plant state score")))
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + theme_bw(base_size=18)
SAV_FP_plot2 <- SAV_FP_plot2 + ggtitle("")
SAV_FP_plot2

ggsave(file="output_for_committee - state_score - non-spatial_nutrients - rectangle- 1ha.jpg",SAV_FP_plot2, height=11,width=8)


###################
# rectangle       #
# large (6.25 ha) #
###################
data_large <- subset(data, data$size=="large")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_large, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Avg. FP cover year 4")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02

SAV_plot02 <- ggplot(data_large, aes(x=TOTALN,y=avg_avg_SAVcover)) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Avg. SAV cover year 4")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output_for_committee - FP&SAV - non-spatial-nutrients - rectangle - 6_25ha.jpg",combined_plot02, height=11,width=8)

##########################
# Re-do the plots above  #
# But with "state_score" #
##########################

###################
# rectangle       #
# large (6.25 ha) #  # try different facetting - see what looks best 
# state_score     #
###################
# Y = average average FP cover for all years (except for first three)
SAV_FP_plot2 <- ggplot(data_large, aes(x=TOTALN,y=state_score))
SAV_FP_plot2 <- SAV_FP_plot2 + scale_colour_grey()
SAV_FP_plot2 <- SAV_FP_plot2 + geom_point(position="jitter",size=3)
SAV_FP_plot2 <- SAV_FP_plot2 + facet_grid(wind_direction ~ wind_shape2)
SAV_FP_plot2 <- SAV_FP_plot2 + xlab("Total N (mg/L)")
SAV_FP_plot2 <- SAV_FP_plot2 + ylab(expression(paste("Plant state score")))
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + theme_bw(base_size=18)
SAV_FP_plot2 <- SAV_FP_plot2 + ggtitle("")
SAV_FP_plot2

ggsave(file="output_for_committee - state_score - non-spatial_nutrients - rectangle- 6_25ha.jpg",SAV_FP_plot2, height=11,width=8)


###################
# rectangle       #
# XL (9 ha) #
###################
data_XL <- subset(data, data$size=="XL")

# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data_XL, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ wind_shape2)
FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Avg. FP cover year 4")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02

SAV_plot02 <- ggplot(data_XL, aes(x=TOTALN,y=avg_avg_SAVcover)) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ wind_shape2)
SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Avg. SAV cover year 4")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02

combined_plot02 <- arrangeGrob(FP_plot02, SAV_plot02,ncol=1, main=NULL,sub=NULL)

ggsave(file="output_for_committee - FP&SAV - non-spatial-nutrients - rectangle - 9ha.jpg",combined_plot02, height=11,width=8)

##########################
# Re-do the plots above  #
# But with "state_score" #
##########################

###################
# rectangle       #
# XL (9 ha) #  # try different facetting - see what looks best 
# state_score     #
###################
# Y = average average FP cover for all years (except for first three)
SAV_FP_plot2 <- ggplot(data_XL, aes(x=TOTALN,y=state_score))
SAV_FP_plot2 <- SAV_FP_plot2 + scale_colour_grey()
SAV_FP_plot2 <- SAV_FP_plot2 + geom_point(position="jitter",size=3)
SAV_FP_plot2 <- SAV_FP_plot2 + facet_grid(wind_direction ~ wind_shape2)
SAV_FP_plot2 <- SAV_FP_plot2 + xlab("Total N (mg/L)")
SAV_FP_plot2 <- SAV_FP_plot2 + ylab(expression(paste("Plant state score")))
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + theme_bw(base_size=18)
SAV_FP_plot2 <- SAV_FP_plot2 + ggtitle("")
SAV_FP_plot2

ggsave(file="output_for_committee - state_score - non-spatial_nutrients - rectangle- 9ha.jpg",SAV_FP_plot2, height=11,width=8)



###################
# rectangle       #
# All sizes       #
# wind_dir=all    #
# wind_shape2=0.2 #
# state_score     #
###################
data02 <- subset(data, data$wind_direction=="all" & data$wind_shape2==0.2)

# re-order size 
data02$size <- factor(data02$size, levels=c("small", "medium", "large", "XL"))

SAV_FP_plot2 <- ggplot(data02, aes(x=TOTALN,y=state_score))
SAV_FP_plot2 <- SAV_FP_plot2 + scale_colour_grey()
SAV_FP_plot2 <- SAV_FP_plot2 + geom_point(position="jitter",size=3)
SAV_FP_plot2 <- SAV_FP_plot2 + facet_grid(. ~ size)
SAV_FP_plot2 <- SAV_FP_plot2 + xlab("Total N (mg/L)")
SAV_FP_plot2 <- SAV_FP_plot2 + ylab(expression(paste("Plant state score")))
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
SAV_FP_plot2 <- SAV_FP_plot2 + theme_bw(base_size=18)
SAV_FP_plot2 <- SAV_FP_plot2 + ggtitle("")
SAV_FP_plot2

ggsave(file="output_for_committee - state_score - non-spatial_nutrients - rectangle - all_sizes - up_0.2.jpg",SAV_FP_plot2, height=8,width=16)








######
#########
########## START HERE 
#########
#######

########################
# Loop through / match #
########################
for (i in 1:unique(data$size)){
  for (j in 1:unique(data$wind_shape2)){
    for (k in 1:unique(data$wind_direction)){
      # should I group them with an ID here? 
      for (l in 1:unique(data$TOTALN)){
        # should I group them with an ID here?
        data$id <- paste(i,j,k,l,sep="_")
        # this is where I need to figure out 
        # for all of the different initial conditions 
        # what is their final state 
        # use something like a re-shape 
        # I want a list for each id 
        # of all the different states
        # then I can evaluate that list 
        # if the list is homogenous, then I can say "FP" or "SAV" or "mixed"
        # if the list is heterogenous, then I can say "ASS" 
      }
    }
  }
}


####################################################
# plot the line segment of "state" along nutrients #
####################################################
ggplot(_____place_holder_______, aes(x=TOTALN,y=state))) 


# not sure how to arrange this in ggplot2
