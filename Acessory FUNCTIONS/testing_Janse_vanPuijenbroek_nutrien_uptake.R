temp <- 20

##################
# Values for SAV #
##################
cVNUptMax <- 0.0385 
rND <- 0.03
cNDmin <- 0.0125 
cNDmax <- 0.04
cQ10Prod <- 1.75
cAffNUpt <- 0.011 

#################
# Values for FP #
#################
cVNUptMax <- 0.0385 
rND <- 0.03
cNDmin <- 0.02
cNDmax <- 0.75
cQ10Prod <- 2.25
cAffNUpt <- 0.011 

##################### 
# Define a function # 
##################### 
uptake_rate <- function(TOTALN){
  # corrected maximum uptake rate 
  uptake_max_cor <- cVNUptMax*cQ10Prod^(0.1*(temp-20))*((cNDmax-rND)/(cNDmax-cNDmin))
  
  # specific uptake rate 
  rate <- uptake_max_cor*(TOTALN/((uptake_max_cor/cAffNUpt)+TOTALN))
  rate
}


######################### 
# Test function for SAV # 
######################### 
mydata_SAV <- matrix(nrow = 1, ncol = 2)

cVNUptMax <- 0.0385 
rND <- 0.03
cNDmin <- 0.0125 
cNDmax <- 0.04
cQ10Prod <- 1.75
cAffNUpt <- 0.011 

for(j in seq(0,10,0.5)){
  mydata_SAV <- rbind(mydata_SAV, c(j,uptake_rate(j)))
}


######################## 
# Test function for FP # 
######################## 
mydata_FP <- matrix(nrow = 1, ncol = 2)

cVNUptMax <- 0.0385 
rND <- 0.03
cNDmin <- 0.02
cNDmax <- 0.75
cQ10Prod <- 2.25
cAffNUpt <- 0.011 

for(j in seq(0,10,0.5)){
  mydata_FP <- rbind(mydata_FP, c(j,uptake_rate(j)))
}


################
# Combine data #
################
mydata_SAV<- as.data.frame(mydata_SAV)
colnames(mydata_SAV) <- c("TOTALN","uptake_rate")
mydata_SAV$veg_type <- "SAV"

mydata_FP <- as.data.frame(mydata_FP)
colnames(mydata_FP) <- c("TOTALN","uptake_rate")
mydata_FP$veg_type <- "FP"

mydata_combined <- rbind(mydata_SAV,mydata_FP)

###########
# plot it #
###########
plot2 <- ggplot(mydata_combined, aes(x=TOTALN, y=uptake_rate, group=veg_type, color=veg_type)) 
plot2 <- plot2 + geom_point() + geom_line()
plot2 <- plot2 + xlab("Total N (mg N/L)")
plot2 <- plot2 + ylab("N uptake rate (mg N/mg D/day)")
plot2 <- plot2 + theme_classic(base_size=18)
plot2 <- plot2 + ggtitle("Temp=20*C; rND=0.03")
plot2

ggsave("N_uptake_rate_vs_TOTALN.jpg",plot2,height=5,width=5)

