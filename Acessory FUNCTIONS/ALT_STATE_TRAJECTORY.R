#########################################################
# Plot trajectory of FP and SAV growth                  # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 7/24/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

data <- read.csv("output26.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)

###################
# Subset          #      
# same parameters #
# same nutrients  #
###################


###################
# Manipulate data #
###################
data02 <- reshape(data,
                  
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
###########
# Plot it #
###########
plot <- ggplot(data02, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
plot <- plot + geom_point() 
plot <- plot + geom_line(arrow=arrow())
plot <- plot + xlab("% SAV cover")
plot <- plot + ylab("% FP cover")
plot <- plot + theme_classic(base_size=18)
plot <- plot + theme(legend.position="none")
plot
