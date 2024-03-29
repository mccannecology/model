#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

# I already combined my results into output34.csv 
data <- read.csv("output_multiple_species.csv")

head(data)
nrow(data)

###############################################
# Calculate "state_score" for each simulation #
# Range (-70.7, 70.7)                         #
# (-) score: more SAV                         #
# (+) score: more FP                          #
###############################################
# I could also use this (avg cover in the final year)
data$state_score04 <- (data$FP_end_yr04 - data$SAV_end_yr04) / sqrt(2)
data$state_score03 <- (data$FP_end_yr03 - data$SAV_end_yr03) / sqrt(2)
data$state_score02 <- (data$FP_end_yr02 - data$SAV_end_yr02) / sqrt(2)
data$state_score01 <- (data$FP_end_yr01 - data$SAV_end_yr01) / sqrt(2)

#################################################### 
# Calculate difference between last two time steps #  
####################################################
data$state_score04vs03 <- data$state_score04 - data$state_score03

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
# Re-do the plots above  #
# But with "state_score" #
##########################

# change "data$scenario" to a factor with different levels 
data$scenario_factor <- as.factor(data$scenario)
levels(data$scenario_factor) <- c("1 species","3 species: equal","3 species: tradeoffs","3 species: hierarchy")

SAV_FP_plot2 <- ggplot(data, aes(x=TOTALN,y=state_score04))
SAV_FP_plot2 <- SAV_FP_plot2 + scale_colour_grey()
SAV_FP_plot2 <- SAV_FP_plot2 + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075)) 
SAV_FP_plot2 <- SAV_FP_plot2 + facet_grid(. ~ scenario_factor)
SAV_FP_plot2 <- SAV_FP_plot2 + xlab("Total N (mg/L)")
SAV_FP_plot2 <- SAV_FP_plot2 + ylab(expression(paste("Plant state score")))
SAV_FP_plot2 <- SAV_FP_plot2 + theme_bw(base_size=18)
SAV_FP_plot2 <- SAV_FP_plot2 + ggtitle("1 ha, rectangle")
SAV_FP_plot2 <- SAV_FP_plot2 + scale_x_continuous(breaks=c(1,3,5,7,9))
SAV_FP_plot2

ggsave(file="output_multiple_species - state_score.jpg",SAV_FP_plot2, height=4,width=10)


# with colour
color_lims <- max(abs(min(data$state_score04vs03)), max(data$state_score04vs03))

SAV_FP_plot2 <- ggplot(data, aes(x=TOTALN,y=state_score04))
SAV_FP_plot2 <- SAV_FP_plot2 + geom_point(size=3, 
                                        aes(fill=state_score04vs03),
                                        position=position_jitter(w=0.075),
                                        colour="black",pch=21)
SAV_FP_plot2 <- SAV_FP_plot2 + scale_fill_gradientn(colours=c("red","white","blue"),
                                                  limits=c(-color_lims,color_lims),
                                                  name=expression(paste(Delta," state",sep="")))
SAV_FP_plot2 <- SAV_FP_plot2 + facet_grid(. ~ scenario_factor)
SAV_FP_plot2 <- SAV_FP_plot2 + xlab("Total N (mg/L)")
SAV_FP_plot2 <- SAV_FP_plot2 + ylab(expression(paste("Plant state score")))
SAV_FP_plot2 <- SAV_FP_plot2 + theme_bw(base_size=18)
SAV_FP_plot2 <- SAV_FP_plot2 + ggtitle(NULL)
SAV_FP_plot2 <- SAV_FP_plot2 + scale_x_continuous(breaks=c(1,3,5,7,9))
SAV_FP_plot2

ggsave(file="output_multiple_species - state_score - with_colour.jpg",SAV_FP_plot2, height=4,width=11)











FP_plot2 <- ggplot(data, aes(x=TOTALN,y=))
FP_plot2 <- FP_plot2 + scale_colour_grey()
FP_plot2 <- FP_plot2 + geom_point(position="jitter",size=3)
FP_plot2 <- FP_plot2 + facet_grid(scenario ~ .)
FP_plot2 <- FP_plot2 + xlab("Total N (mg/L)")
FP_plot2 <- FP_plot2 + ylab(expression(paste("Plant state score")))
FP_plot2 <- FP_plot2 + geom_hline(yintercept=(50/sqrt(2)),colour="red",linetype="dashed")
FP_plot2 <- FP_plot2 + geom_hline(yintercept=(-50/sqrt(2)),colour="red",linetype="dashed")
FP_plot2 <- FP_plot2 + theme_bw(base_size=18)
FP_plot2 <- FP_plot2 + ggtitle("")
FP_plot2









###################
# Y = average average FP cover for all years (except for first three)
FP_plot02 <- ggplot(data, aes(x=TOTALN,y=avg_avg_FPcover)) 
FP_plot02 <- FP_plot02 + scale_colour_grey()
FP_plot02 <- FP_plot02 + geom_point(position="jitter",size=3)
FP_plot02 <- FP_plot02 + facet_grid(wind_direction ~ size)
#FP_plot02 <- FP_plot02 + ylim(0,100)
FP_plot02 <- FP_plot02 + xlab("Total N (mg/L)")
FP_plot02 <- FP_plot02 + ylab(expression(paste("Avg. FP cover year 4")))
FP_plot02 <- FP_plot02 + theme_bw(base_size=18)
FP_plot02
ggsave(file="output_wind_and_size - FP.jpg",FP_plot02, height=8,width=11)


SAV_plot02 <- ggplot(data, aes(x=TOTALN,y=avg_avg_SAVcover)) 
SAV_plot02 <- SAV_plot02 + scale_colour_grey()
SAV_plot02 <- SAV_plot02 + geom_point(position="jitter",size=3)
SAV_plot02 <- SAV_plot02 + facet_grid(wind_direction ~ size)
#SAV_plot02 <- SAV_plot02 + ylim(0,100)
SAV_plot02 <- SAV_plot02 + xlab("Total N (mg/L)")
SAV_plot02 <- SAV_plot02 + ylab(expression(paste("Avg. SAV cover year 4")))
SAV_plot02 <- SAV_plot02 + theme_bw(base_size=18)
SAV_plot02
ggsave(file="output_wind_and_size - FP.jpg",SAV_plot02, height=8,width=11)


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
