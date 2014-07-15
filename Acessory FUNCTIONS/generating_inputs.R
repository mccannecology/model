#####################
# Varying Paramters #
##################### 
TOTALN<-c(0.1,0.5,seq(1,10,1))
area <- c(400,1600,3600)
HtoW <- c(1,0.25,4)
wind_direction <- c("all","up")
wind_shape2 <- c(4, 0.4, 0.2)

temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(area)){
    for(k in 1:length(HtoW)){
      for (l in 1:length(wind_direction)){
        for (n in 1:length(wind_shape2)){
            
           temp2 <- c(TOTALN[i],
                      area[j],
                      HtoW[k],
                      wind_direction[l],
                      wind_shape2[n]
                       )
           
            temp <- rbind(temp,temp2)
         
       }
      }
    }
  }
}

colnames(temp) <- c("TOTALN",
                    "area",
                    "HtoW",
                    "wind_direction",
                    "wind_shape2"
                    )

temp <- as.data.frame(temp)

# things that will be defined conditionally
temp$prob_up <- ifelse(temp$wind_direction=="up", 0.7,0.25)
temp$prob_down <- ifelse(temp$wind_direction=="up", 0.1,0.25) 
temp$prob_left <- ifelse(temp$wind_direction=="up", 0.1,0.25) 	
temp$prob_right <- ifelse(temp$wind_direction=="up", 0.1,0.25) 

write.csv(temp,file="temp.csv",row.names=F)
