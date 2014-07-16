#####################
# Varying Paramters #
##################### 
TOTALN<-c(0.1,0.5,seq(1,10,1))
initial_perc_SAV_cover<-c(1, 15, 30, 60)
initial_perc_FP_cover<-c(1, 15, 30, 60)


temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(initial_perc_SAV_cover)){
    for(k in 1:length(initial_perc_FP_cover)){
            
           temp2 <- c(TOTALN[i],
                      initial_perc_SAV_cover[j],
                      initial_perc_FP_cover[k]
                       )
           
            temp <- rbind(temp,temp2)

    }
  }
}

colnames(temp) <- c("TOTALN",
                    "initial_perc_SAV_cover",
                    "initial_perc_FP_cover"
                    )

write.csv(temp,file="temp.csv",row.names=F)
