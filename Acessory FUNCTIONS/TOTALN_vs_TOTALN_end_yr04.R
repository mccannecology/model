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
SAV_FP_plot <- SAV_FP_plot + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.075))                                    
SAV_FP_plot <- SAV_FP_plot + xlab("Initial Total N (mg/L)")
SAV_FP_plot <- SAV_FP_plot + ylab(expression(paste("Plant state score")))
SAV_FP_plot <- SAV_FP_plot + geom_hline(yintercept=0)
SAV_FP_plot <- SAV_FP_plot + theme_bw(base_size=18)
SAV_FP_plot <- SAV_FP_plot + scale_x_continuous(breaks=c(1,3,5,7,9))
SAV_FP_plot <- SAV_FP_plot + annotate("text",x=0.75,y=77,label="a)")
SAV_FP_plot 


# plot the final nutrient levels vs. the initial nutrient levels 
SAV_FP_plot2 <- ggplot(temp_data, aes(x=TOTALN_end_yr04,y=state_score))
SAV_FP_plot2 <- SAV_FP_plot2 + geom_point(alpha=0.2,size=3, position=position_jitter(w=0.025,h=1))                                    
SAV_FP_plot2 <- SAV_FP_plot2 + xlab("Current Total N (mg/L)")
SAV_FP_plot2 <- SAV_FP_plot2 + ylab(expression(paste("Plant state score")))
SAV_FP_plot2 <- SAV_FP_plot2 + geom_hline(yintercept=0)
SAV_FP_plot2 <- SAV_FP_plot2 + theme_bw(base_size=18)
SAV_FP_plot2 <- SAV_FP_plot2 + annotate("text",x=-0.05,y=77,label="b)")
SAV_FP_plot2



combo_plot <- arrangeGrob(SAV_FP_plot, SAV_FP_plot2,ncol=1)
combo_plot

ggsave("TOTALN_vs_TOTALN_end_yr04 - default_settings.jpg",combo_plot,height=10,width=5)

