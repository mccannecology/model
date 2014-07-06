# neigh_thresh_SAV    50,100,200
# focal_thresh_SAV	  0,50,100
# amnt_colonize_SAV	  1,5
# neigh_thresh_FP	    50,100,200
# focal_thresh_FP	    0,50,100
# amnt_colonize_FP	  1,5

neigh_thresh_SAV <- c(50,100,200)
focal_thresh_SAV <- c(0,50,100)
amnt_colonize_SAV	<- c(1,5)
neigh_thresh_FP	<- c(50,100,200)
focal_thresh_FP	<- c(0,50,100)
amnt_colonize_FP <- c(1,5)

temp <- NULL

for (i in 1:length(neigh_thresh_SAV)){
  for (j in 1:length(focal_thresh_SAV)){
    for(k in 1:length(amnt_colonize_SAV)){
      for (l in 1:length(neigh_thresh_FP)){
        for (m in 1:length(focal_thresh_FP)){
          for (n in 1:length(amnt_colonize_FP)){
            temp2 <- c(neigh_thresh_SAV[i],
                       focal_thresh_SAV[j],
                       amnt_colonize_SAV[k],
                       neigh_thresh_FP[l],
                       focal_thresh_FP[m],
                       amnt_colonize_FP[n])
           temp <- rbind(temp,temp2)
          }
        }
      }
    }
  }
}

colnames(temp) <- c("neigh_thresh_SAV",
                    "focal_thresh_SAV",
                    "amnt_colonize_SAV",
                    "neigh_thresh_FP",
                    "focal_thresh_FP",
                    "amnt_colonize_FP")

write.csv(temp,file="temp.csv",row.names=F)
