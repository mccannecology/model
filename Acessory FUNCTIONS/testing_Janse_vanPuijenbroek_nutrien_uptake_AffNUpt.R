##################### 
# Define a function # 
##################### 
uptake_rate <- function(TOTALN, rND, cAffNUpt, uptake_max_cor){
  # corrected maximum uptake rate 
  # uptake_max_cor <- cVNUptMax*cQ10Prod^(0.1*(temp-20))*((cNDmax-rND)/(cNDmax-cNDmin))
  
  # specific uptake rate 
  rate <- uptake_max_cor*(TOTALN/((uptake_max_cor/cAffNUpt)+TOTALN))
  rate
}


######################### 
# Test function for SAV # 
######################### 
mydata_SAV<- matrix(nrow = 1, ncol = 4)

temp <- 20 # temperature 
cVNUptMax <- 0.07 # max uptake rate 
cNDmin <- 0.02  # min. ratio of nitrogen to plant dry mass
cNDmax <- 0.05 # max. ratio of nitrogen to plant dry mass
cQ10Prod <- 2 # factor by which growth rate increases due to a 10C increase in temp 
rND <- 0.02

for(j in seq(0,10,0.5)){
  for(k in seq(0.02,0.07,0.005)){
    for(l in seq(0.02,0.07,0.005)){
      mydata_SAV <- rbind(mydata_SAV, c(j,k,l,uptake_rate(TOTALN=j, 
                                                          rND=rND, 
                                                          cAffNUpt=k,
                                                          uptake_max_cor=l)))
    }
  }
}


######################## 
# Test function for FP # 
######################## 
mydata_FP <- matrix(nrow = 1, ncol = 4)

temp <- 20
cVNUptMax <- 0.07
cNDmin <- 0.03
cNDmax <- 0.1
cQ10Prod <- 2.5

for(j in seq(0,10,0.5)){
  for(k in seq(0.02,0.07,0.005)){
    for(l in seq(0.02,0.07,0.005)){
      mydata_FP <- rbind(mydata_FP, c(j,k,l,uptake_rate(TOTALN=j, 
                                                          rND=rND, 
                                                          cAffNUpt=k,
                                                          uptake_max_cor=l)))
    }
  }
}


################
# Combine data #
################
mydata_SAV<- as.data.frame(mydata_SAV)
colnames(mydata_SAV) <- c("TOTALN","cAffNUpt","uptake_max_cor","uptake_rate")
mydata_SAV$veg_type <- "SAV"

mydata_FP <- as.data.frame(mydata_FP)
colnames(mydata_FP) <- c("TOTALN","cAffNUpt","uptake_max_cor","uptake_rate")
mydata_FP$veg_type <- "FP"

mydata_combined <- rbind(mydata_SAV,mydata_FP)

mydata_combined <- mydata_combined[complete.cases(mydata_combined),]

###########
# plot it #
###########
plot_FP_line <- ggplot(subset(mydata_combined, mydata_combined$veg_type=="FP"), 
                       aes(x=TOTALN, y=uptake_rate)) 
plot_FP_line <- plot_FP_line + geom_point() + geom_line()
plot_FP_line <- plot_FP_line + xlab("Total N (mg N/L)")
plot_FP_line <- plot_FP_line + ylab("Uptake rate (gN/gD/day)")
plot_FP_line <- plot_FP_line + facet_grid(cAffNUpt ~ uptake_max_cor)
plot_FP_line <- plot_FP_line + theme_bw(base_size=18)
plot_FP_line <- plot_FP_line + ggtitle("FP")
plot_FP_line
ggsave("N_uptake_rate_vs_TOTALN_AffNUpt_FP.jpg",plot_FP_line,height=5,width=8)


plot_SAV_line <- ggplot(subset(mydata_combined, mydata_combined$veg_type=="SAV"), 
                       aes(x=TOTALN, y=uptake_rate)) 
plot_SAV_line <- plot_SAV_line + geom_point() + geom_line()
plot_SAV_line <- plot_SAV_line + xlab("Total N (mg N/L)")
plot_SAV_line <- plot_SAV_line + ylab("Uptake rate (gN/gD/day)")
plot_SAV_line <- plot_SAV_line + facet_grid(cAffNUpt ~ uptake_max_cor)
plot_SAV_line <- plot_SAV_line + theme_bw(base_size=18)
plot_SAV_line <- plot_SAV_line + ggtitle("SAV")
plot_SAV_line
ggsave("N_uptake_rate_vs_TOTALN_AffNUpt_SAV.jpg",plot_SAV_line,height=5,width=8)


################################ 
# Single plot for presentation #
################################
single_plot <- ggplot(subset(mydata_combined, mydata_combined$veg_type=="SAV" &
                               mydata_combined$uptake_max_cor==0.045 &
                               mydata_combined$cAffNUpt==0.045 ), 
                        aes(x=TOTALN, y=uptake_rate)) 
single_plot <- single_plot + geom_line()
single_plot <- single_plot + xlab("Total N (mg N/L)")
single_plot <- single_plot + ylab("Uptake rate (gN/gD/day)")
single_plot <- single_plot + theme_bw(base_size=18)
single_plot

ggsave("N_uptake_rate_vs_TOTALN_AffNUpt_singlevalueforpresentation.jpg",single_plot,height=5,width=8)

