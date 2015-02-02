#########################################################
# Summarize results from multiple simulations           # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 3/19/2014                                    #
######################################################### 

# This is an example of spatially explicit nutrients 

library(ggplot2)
library(gridExtra)

# If the results have already been combined 
data <- read.csv("output_testing_nutrient_uptake.csv")

head(data)
nrow(data)

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


###########
# Plot it # 
###########
SAV_FP_plot <- ggplot(data, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                    
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot

ggsave(file="testing_nutrient_uptake.jpg",SAV_FP_plot, height=8,width=8)


