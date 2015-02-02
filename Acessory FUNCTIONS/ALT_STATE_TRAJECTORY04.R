#########################################################
# Plot trajectory of FP and SAV growth                  # 
#                                                       #
# By: Michael J. McCann                                 #
# Created: 7/24/2014                                    #
######################################################### 

library(ggplot2)
library(gridExtra)

# this assumes that if you split your simulation into multiple batches,
# that you've already combined them into one .csv file
# see "combine_output_files.R" for combining 

data <- read.csv("output_testing_nutrient_uptake.csv") # imports parameter  values for all simulations 
head(data)
nrow(data)
colnames(data)

#########################################
# Generate plot for each nutrient level #
#########################################
# For loop through variables to subset data 
for (i in unique(data$TOTALN)){
  
  temp_data <- subset(data, data$TOTALN == i)
  
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
  temp_plot <- ggplot(temp_data_reshape, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
  temp_plot <- temp_plot + geom_point() 
  temp_plot <- temp_plot + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
  temp_plot <- temp_plot + xlab("% SAV cover")
  temp_plot <- temp_plot + ylab("% FP cover")
  temp_plot <- temp_plot + ylim(0,100)
  temp_plot <- temp_plot + xlim(0,100)
  temp_plot <- temp_plot + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
  temp_plot <- temp_plot + theme_classic(base_size=16)
  temp_plot <- temp_plot + theme(legend.position="none")
  temp_plot <- temp_plot + ggtitle(paste("Total N: ",i," mg/L",sep=""))
  temp_plot
    
  ggsave(file=paste("alt_states_plot",i,"TOTALN.jpg",sep="_"),temp_plot,height=6,width=6)

}

############################## 
# replot for a combined plot # 
############################## 
for (i in unique(data$TOTALN)){
  
  temp_data <- subset(data, data$TOTALN == i)
  
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
  temp_plot <- ggplot(temp_data_reshape, aes(x=SAV_cover,y=FP_cover,group=id,colour=as.factor(id))) 
  temp_plot <- temp_plot + geom_point() 
  temp_plot <- temp_plot + geom_path(arrow=arrow(type="closed",length=unit(0.15,"inches")),alpha=0.4)
  temp_plot <- temp_plot + xlab("")
  temp_plot <- temp_plot + ylab("")
  temp_plot <- temp_plot + ylim(0,100)
  temp_plot <- temp_plot + xlim(0,100)
  temp_plot <- temp_plot + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
  temp_plot <- temp_plot + theme_classic(base_size=12)
  temp_plot <- temp_plot + theme(legend.position="none")
  temp_plot <- temp_plot + ggtitle(paste(i," mg/L",sep=""))
  temp_plot
  
  assign(paste("plot_",i,sep=""),temp_plot,envir = .GlobalEnv)  
}
########################## 
# Arrange plots into one # 
##########################

combined_plot <- arrangeGrob(plot_1,plot_2,plot_3,plot_4,plot_5,plot_6,plot_7,plot_8,plot_9,ncol=9)
combined_plot

ggsave("combined_alt_state_plots.jpg",combined_plot,height=2,width=16)



combined_plot2 <- arrangeGrob(plot_1,plot_4,plot_9,ncol=3)
combined_plot2

ggsave("combined_alt_state_plots_select_levels.jpg",combined_plot2,height=2,width=6)

