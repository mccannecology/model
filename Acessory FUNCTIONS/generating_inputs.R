#####################
# Varying Paramters #
##################### 
TOTALN<-c(0.1,0.5,seq(1,10,1))
lightlimitationSAV <- c(0.1,0.2)
lightlimitationFP <- c(0.1,0.2)
shadingbyFP <- c(0.02,0.04)

temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(lightlimitationSAV)){
    for(k in 1:length(lightlimitationFP)){
      for (l in 1:length(shadingbyFP)){
       
            temp2 <- c(TOTALN[i],
                       lightlimitationSAV[j],
                       lightlimitationFP[k],
                       shadingbyFP[l]
                       )
           
            temp <- rbind(temp,temp2)

      }
    }
  }
}

colnames(temp) <- c("TOTALN",
                    "lightlimitationSAV",
                    "lightlimitationFP",
                    "shadingbyFP"
                    )

write.csv(temp,file="temp.csv",row.names=F)
