library(ggplot2)
library(gridExtra)

###############
# Import data #
###############
data <- read.csv("output_growing_season.csv")

head(data)
nrow(data)


# Set ap altern. measures of FP&SAV for 600 day growing season  
# data$FP_final[data$years==1] <- data$FP_end_yr01[data$years==1]
# data$FP_final[data$years!=1] <- data$FP_end_yr04[data$years!=1]

# data$SAV_final[data$years==1] <- data$SAV_end_yr01[data$years==1]
# data$SAV_final[data$years!=1] <- data$SAV_end_yr04[data$years!=1]


###############################################
# Calculate "state_score" for each simulation #
# Range (-70.7, 70.7)                         #
# (-) score: more SAV                         #
# (+) score: more FP                          #
###############################################
# I could also use this (avg cover in the final year)
data$state_score <- (data$FP_end_yr04 - data$SAV_end_yr04) / sqrt(2)

# change "data$days" to a factor with different levels 
data$days_factor <- as.factor(data$days)
levels(data$days_factor) <- c("12 years\n 50 days per year","8 years\n 75 days per year",
                              "5 years\n 150 days per year","1 year\n 600 days per year")

# Plot 
SAV_FP_plot <- ggplot(data, aes(x=TOTALN,y=state_score))
SAV_FP_plot <- SAV_FP_plot + scale_colour_grey()
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                 
SAV_FP_plot <- SAV_FP_plot + facet_grid(. ~ days_factor)
SAV_FP_plot <- SAV_FP_plot + xlab("Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + ggtitle("1 ha, rectangle")
SAV_FP_plot

ggsave(file="output_growing_season_state_score.jpg",SAV_FP_plot, height=4,width=12)
