#####################
# Varying Paramters #
##################### 
TOTALN<-c(0.1,0.5,seq(1,10,1))
initial_perc_SAV_cover<-c(15, 30)
initial_perc_FP_cover<-c(15, 30)
neigh_thresh_SAV <- c(50,100,200)  
focal_thresh_SAV <- c(0,50,100)	
neigh_thresh_FP	<- c(50,100,200)  
focal_thresh_FP	<- c(0,50,100)
wind_shape2 <- c(4, 0.2)

temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(initial_perc_SAV_cover)){
    for(k in 1:length(initial_perc_FP_cover)){
      for(l in 1:length(neigh_thresh_SAV)){
        for(m in 1:length(focal_thresh_SAV)){
          for(n in 1:length(neigh_thresh_FP)){
            for(o in 1:length(focal_thresh_FP)){
              for(p in 1:length(wind_shape2)){
                
                temp2 <- c(TOTALN[i],
                      initial_perc_SAV_cover[j],
                      initial_perc_FP_cover[k],
                      neigh_thresh_SAV[l],
                      focal_thresh_SAV[m],
                      neigh_thresh_FP[n],
                      focal_thresh_FP[o],
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
}

colnames(temp) <- c("TOTALN",
                    "initial_perc_SAV_cover",
                    "initial_perc_FP_cover",
                    "neigh_thresh_SAV",
                    "focal_thresh_SAV",
                    "neigh_thresh_FP",
                    "focal_thresh_FP",
                    "wind_shape2"
                    )

write.csv(temp,file="temp.csv",row.names=F)
