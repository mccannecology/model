# Larger Lakes
# for committee meeting - 9/15/2014

#####################
# Varying Paramters #
##################### 
TOTALN<-c(seq(1,9,1))
initial_perc_SAV_cover<-c(5,15,30)
initial_perc_FP_cover<-c(5,15,30)
wind_direction <- c("all","up")
wind_shape2 <- c(4, 0.2, 0.1)
size <- c("small","medium","large","XL")


temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(initial_perc_SAV_cover)){
    for(k in 1:length(initial_perc_FP_cover)){
      for(l in 1:length(wind_direction)){
        for(m in 1:length(wind_shape2)){
          for(n in 1:length(size)){
              
              temp2 <- c(TOTALN[i],
                    initial_perc_SAV_cover[j],
                    initial_perc_FP_cover[k],
                    wind_direction[l],
                    wind_shape2[m],
                    size[n]
                    )
         
              temp <- rbind(temp,temp2)
           
              
          }
        }
      }
    }        
  }
}


colnames(temp) <- c("TOTALN",
                    "initial_perc_SAV_cover",
                    "initial_perc_FP_cover",
                    "wind_direction",
                    "wind_shape2",
                    "size"
                    )

write.csv(temp,file="temp.csv",row.names=F)
