##################### 
# Define a function # 
##################### 
uptake_rate <- function(TOTALN, rND){
  # corrected maximum uptake rate 
  uptake_max_cor <- cVNUptMax*cQ10Prod^(0.1*(temp-20))*((cNDmax-rND)/(cNDmax-cNDmin))
  
  # specific uptake rate 
  rate <- uptake_max_cor*(TOTALN/((uptake_max_cor/cAffNUpt)+TOTALN))
  rate
}


######################### 
# Test function for SAV # 
######################### 
mydata_SAV<- matrix(nrow = 1, ncol = 3)

temp <- 20 # temperature 
cVNUptMax <- 0.07 # max uptake rate 
cNDmin <- 0.02  # min. ratio of nitrogen to plant dry mass
cNDmax <- 0.05 # max. ratio of nitrogen to plant dry mass
cQ10Prod <- 2 # factor by which growth rate increases due to a 10C increase in temp 
cAffNUpt <- 0.02 # affinity for N [m3/gD/d]

for(j in seq(0,10,0.5)){
  for(k in seq(0.02,0.05,0.005)){
    mydata_SAV <- rbind(mydata_SAV, c(j,k,uptake_rate(TOTALN=j,rND=k)))
  }
}


######################## 
# Test function for FP # 
######################## 
mydata_FP <- matrix(nrow = 1, ncol = 3)

temp <- 20
cVNUptMax <- 0.07
cNDmin <- 0.03
cNDmax <- 0.1
cQ10Prod <- 2.5
cAffNUpt <- 0.02

for(j in seq(0,10,0.5)){
  for(k in seq(0.03,0.1,0.01)){
    mydata_FP <- rbind(mydata_FP, c(j,k,uptake_rate(TOTALN=j,rND=k)))
  }
}


################
# Combine data #
################
mydata_SAV<- as.data.frame(mydata_SAV)
colnames(mydata_SAV) <- c("TOTALN","rND","uptake_rate")
mydata_SAV$veg_type <- "SAV"

mydata_FP <- as.data.frame(mydata_FP)
colnames(mydata_FP) <- c("TOTALN","rND","uptake_rate")
mydata_FP$veg_type <- "FP"

mydata_combined <- rbind(mydata_SAV,mydata_FP)

###########
# plot it #
###########
plot_FP <- ggplot(subset(mydata_combined, mydata_combined$veg_type=="FP"), aes(TOTALN, rND)) 
plot_FP <- plot_FP + geom_tile(aes(fill=uptake_rate))
plot_FP <- plot_FP + xlab("Total N (mg N/L)")
plot_FP <- plot_FP + ylab("rND (gN/gDW)")
plot_FP <- plot_FP + theme_classic(base_size=18)
plot_FP

plot_SAV <- ggplot(subset(mydata_combined, mydata_combined$veg_type=="SAV"), aes(TOTALN, rND)) 
plot_SAV <- plot_SAV + geom_tile(aes(fill=uptake_rate))
plot_SAV <- plot_SAV + xlab("Total N (mg N/L)")
plot_SAV <- plot_SAV + ylab("rND (gN/gDW)")
plot_SAV <- plot_SAV + theme_classic(base_size=18)
plot_SAV


plot_FP_line <- ggplot(subset(mydata_combined, mydata_combined$veg_type=="FP"), 
                       aes(x=TOTALN, y=uptake_rate, group=as.factor(rND), colour=as.factor(rND))) 
plot_FP_line <- plot_FP_line + geom_point() + geom_line()
plot_FP_line <- plot_FP_line + xlab("Total N (mg N/L)")
plot_FP_line <- plot_FP_line + ylab("Uptake rate (gN/gD/day)")
plot_FP_line <- plot_FP_line + labs(colour="rND (gN/gD)")
plot_FP_line <- plot_FP_line + theme_classic(base_size=18)
plot_FP_line <- plot_FP_line + ggtitle("FP")
plot_FP_line
ggsave("N_uptake_rate_vs_TOTALN_rND_FP.jpg",plot_FP_line,height=5,width=8)


plot_SAV_line <- ggplot(subset(mydata_combined, mydata_combined$veg_type=="SAV"), 
                       aes(x=TOTALN, y=uptake_rate, group=as.factor(rND), colour=as.factor(rND))) 
plot_SAV_line <- plot_SAV_line + geom_point() + geom_line()
plot_SAV_line <- plot_SAV_line + xlab("Total N (mg N/L)")
plot_SAV_line <- plot_SAV_line + ylab("Uptake rate (gN/gD/day)")
plot_SAV_line <- plot_SAV_line + labs(colour="rND (gN/gD)")
plot_SAV_line <- plot_SAV_line + ggtitle("SAV")
plot_SAV_line <- plot_SAV_line + theme_classic(base_size=18)
plot_SAV_line
ggsave("N_uptake_rate_vs_TOTALN_rND_SAV.jpg",plot_SAV_line,height=5,width=8)






