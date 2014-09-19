# for a test of mixing rates
# 09-19-2014
 
#####################
# Varying Paramters #
##################### 
TOTALN<-c(seq(1,9,1))
initial_perc_SAV_cover<-c(1,5,15,30,45)
initial_perc_FP_cover<-c(1,5,15,30,45)
mix_freq <- c(1,30,60,120)


temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(initial_perc_SAV_cover)){
    for(k in 1:length(initial_perc_FP_cover)){
      for(l in 1:length(mix_freq)){
              
              temp2 <- c(TOTALN[i],
                    initial_perc_SAV_cover[j],
                    initial_perc_FP_cover[k],
                    mix_freq[l]
                    )
         
              temp <- rbind(temp,temp2)
           
      }
    }        
  }
}


colnames(temp) <- c("TOTALN",
                    "initial_perc_SAV_cover",
                    "initial_perc_FP_cover",
                    "mix_freq"
                    )

write.csv(temp,file="temp.csv",row.names=F)
