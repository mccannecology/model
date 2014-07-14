#####################
# Varying Paramters #
##################### 
TOTALN<-c(0.1,0.5,seq(1,10,1))
lightlimitationSAV <- c(0.005, 0.01, 0.015)
lightlimitation01 <- c(0.005, 0.01, 0.015)
shadingbyFP <- c(0.01, 0.02, 0.03)

temp <- NULL

for (i in 1:length(TOTALN)){
  for (j in 1:length(lightlimitationSAV)){
    for(k in 1:length(lightlimitation01)){
      for (l in 1:length(shadingbyFP)){
       
            temp2 <- c(TOTALN[i],
                       lightlimitationSAV[j],
                       lightlimitation01[k],
                       shadingbyFP[l]
                       )
           
            temp <- rbind(temp,temp2)

      }
    }
  }
}

colnames(temp) <- c("TOTALN",
                    "lightlimitationSAV",
                    "lightlimitation01",
                    "shadingbyFP"
                    )

write.csv(temp,file="temp.csv",row.names=F)
