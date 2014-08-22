#####################
# Varying Paramters #
##################### 
TOTALN<-c(0.1,0.5,seq(1,10,1))
initial_perc_SAV_cover<-c(1,5,15,30,45)
initial_perc_FP_cover<-c(1,5,15,30,45)
shadingbyFP <- c(0.02,0.04,0.08)
shape <- c("rectangle", "hook", "tee", "eight", "cross")
wind_direction <- c("all","up")
wind_shape2 <- c(4, 0.2)

temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(initial_perc_SAV_cover)){
    for(k in 1:length(initial_perc_FP_cover)){
      for(l in 1:length(shadingbyFP)){
        for(n in 1:length(shape)){
          for(o in 1:length(wind_direction)){
            for(p in 1:length(wind_shape2)){
              
              temp2 <- c(TOTALN[i],
                    initial_perc_SAV_cover[j],
                    initial_perc_FP_cover[k],
                    shadingbyFP[l],
                    shape[n],
                    wind_direction[o],
                    wind_shape2[p]
                    )
         
              temp <- rbind(temp,temp2)
           
              
            }
          }
        }
      }        
    }
  }
}

colnames(temp) <- c("TOTALN",
                    "initial_perc_SAV_cover",
                    "initial_perc_FP_cover",
                    "shadingbyFP",
                    "shape",
                    "wind_direction",
                    "wind_shape2"
                    )

write.csv(temp,file="temp.csv",row.names=F)
