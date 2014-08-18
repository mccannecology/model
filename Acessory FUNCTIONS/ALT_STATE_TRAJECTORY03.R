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
colnames(data)

############################################
# Generate plot for shiny version of model #
############################################

# For loop through variables to subset data 


for (i in unique(data$TOTALN)){
  for (j in unique(data$shadingbyFP)){
    for (k in unique(data$size)){
      for (l in unique(data$shape)){
        for (m in unique(data$wind_direction)){
          for (n in unique(data$wind_shape2)){
          
            temp_data <- subset(data, 
                                data$TOTALN == i & 
                                  data$shadingbyFP == j & 
                                  data$size == k & 
                                  data$shape == l &
                                  data$wind_direction == m &
                                  data$wind_shape2 == n)
            
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
            temp_plot <- temp_plot + xlab("% SAV cover")
            temp_plot <- temp_plot + ylab("% FP cover")
            temp_plot <- temp_plot + ylim(0,100)
            temp_plot <- temp_plot + xlim(0,100)
            temp_plot <- temp_plot + geom_segment(x=0,y=0,xend=100,yend=100, colour="black",linetype="dashed")
            temp_plot <- temp_plot + theme_classic(base_size=16)
            temp_plot <- temp_plot + theme(legend.position="none")
            
            ggsave(file=paste("alt_plot_",i,j,k,l,m,n,".jpg",sep="_"),temp_plot,height=6,width=6)

          }
        }
      }
    }
  }
}


##############################
# Building the look up table #
# Have input parameters      #
# Want to find plot number   #   
##############################
TOTALN<-c(0.1,0.5,seq(1,10,1))
shadingbyFP <- c(0.02,0.04,0.08)
size <- c("small","large")
shape <- c("rectangle", "hook", "tee", "eight", "cross")
wind_direction <- c("all","up")
wind_shape2 <- c(4, 0.2)

temp <- NULL

for (i in 1:length(TOTALN)){
  for(j in 1:length(shadingbyFP)){
    for(k in 1:length(size)){
      for(l in 1:length(shape)){
        for(m in 1:length(wind_direction)){
          for(n in 1:length(wind_shape2)){
            
            temp <- rbind(temp,
                       c(TOTALN[i],
                       shadingbyFP[j],
                       size[k],
                       shape[l],
                       wind_direction[m],
                       wind_shape2[n]
                       )
            )
            
          }
        }
      }
    }
  }        
}

temp <- as.data.frame(temp)

colnames(temp) <- c("TOTALN",
                    "shadingbyFP",
                    "size",
                    "shape",
                    "wind_direction",
                    "wind_shape2"
                    )

temp$id <- seq(1,nrow(temp),1)

head(temp)
nrow(temp)


