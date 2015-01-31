##################
# Values for SAV #
##################
# I used the median between the min. and max. specified by Janse & van Puijenbroek 1997
temp <- 20 # temperature 
cVNUptMax <- 0.0385 # max uptake rate 
#rND <- 0.03 # current ratio of nitrogen to plant dry mass
cNDmin <- 0.0125  # min. ratio of nitrogen to plant dry mass
cNDmax <- 0.04 # max. ratio of nitrogen to plant dry mass
cQ10Prod <- 1.75 # factor by which growth rate increases due to a 10C increase in temp 
cAffNUpt <- 0.011 # affinity for N [m3/gD/d]

#################
# Values for FP #
#################
temp <- 20
cVNUptMax <- 0.0385 
#rND <- 0.03
cNDmin <- 0.02
cNDmax <- 0.75
cQ10Prod <- 2.25
cAffNUpt <- 0.011 

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

# for new biomass
mydata_SAV_new <- matrix(nrow = 1, ncol = 2)

cVNUptMax <- 0.0385 
cNDmin <- 0.0125 
cNDmax <- 0.04
cQ10Prod <- 1.75
cAffNUpt <- 0.011 

for(j in seq(0,10,0.5)){
  mydata_SAV_new <- rbind(mydata_SAV_new, c(j,uptake_rate(TOTALN=j,rND=0.0125)))
}

# for old biomass
mydata_SAV_old <- matrix(nrow = 1, ncol = 2)

cVNUptMax <- 0.0385 
cNDmin <- 0.0125 
cNDmax <- 0.04
cQ10Prod <- 1.75
cAffNUpt <- 0.011 

for(j in seq(0,10,0.5)){
  mydata_SAV_old <- rbind(mydata_SAV_old, c(j,uptake_rate(TOTALN=j,rND=0.03)))
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

