# Wind strength tied to size 
# 9/19/2014

#####################
# Varying Paramters #
##################### 
TOTALN<-c(seq(1,9,1))
initial_perc_SAV_cover<-c(1,5,15,30,45)
initial_perc_FP_cover<-c(1,5,15,30,45)
wind_direction <- c("all","up")
size <- c("small","medium","large","XL")


temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(initial_perc_SAV_cover)){
    for(k in 1:length(initial_perc_FP_cover)){
      for(l in 1:length(wind_direction)){
          for(n in 1:length(size)){
              
            temp2 <- c(TOTALN[i],
                  initial_perc_SAV_cover[j],
                  initial_perc_FP_cover[k],
                  wind_direction[l],
                  size[n]
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
                    "size"
                    )

write.csv(temp,file="temp.csv",row.names=F)
