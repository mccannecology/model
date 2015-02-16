# Ditches 
# 02/16/2015

#####################
# Varying Paramters #
##################### 
TOTALN<-c(seq(1,9,1))
initial_perc_SAV_cover<-c(1,5,15,30)
initial_perc_FP_cover<-c(1,5,15,30)
wind_direction <- c("all","up","right")
wind_shape1 <- c(0.01, 0.5)


temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(initial_perc_SAV_cover)){
    for(k in 1:length(initial_perc_FP_cover)){
      for(l in 1:length(wind_direction)){
          for(n in 1:length(wind_shape1)){
              
            temp2 <- c(TOTALN[i],
                  initial_perc_SAV_cover[j],
                  initial_perc_FP_cover[k],
                  wind_direction[l],
                  wind_shape1[n]
                  )
       
            temp <- rbind(temp,temp2)

        }
      }
    }        
  }
}


colnames(temp) <- c("TOTALN",
                    "initial_perc_SAV_cover",
                    "initial_perc_FP_cover",
                    "wind_direction",
                    "wind_shape1"
                    )

write.csv(temp,file="temp.csv",row.names=F)
