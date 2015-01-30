#########################################################
# Outputs model results                                 #
# Replaces functions ANIMATE() and PLOT()               #
#                                                       #
# Compatible with new LIST structure                    #      
# Compatible with SAV component                         #
#                                                       # 
# By: Michael J. McCann                                 #
# Last Updated: 7/8/2014                                #
######################################################### 

########################################
# load workspace for de-bugging
#
# complete LIST 
# 7 years, 125 days each 
# load("testworkspace-complete.Rdata")
########################################

OUTPUT_28 <- function(shapshots=FALSE,timeseries=FALSE){
  
  ################################## 
  # Setting up folders for outputs # 
  ##################################
  # save your original working directory
  original_wd <- getwd()
  
  # then change the working directory for your output files to /OUTPUT 
  setwd(paste(getwd(),"/OUTPUT",sep=""))
  
  # create another folder to hold the animation results - inside the /OUTPUT folder 
  # dir.create(paste(format(Sys.time(), "%m-%d-%Y-%H%M")," - simul",simulnumb, sep=""))
  # without the time stamp
  dir.create(paste(getwd(),"/",parameters$simulation[simulnumb],sep=""),showWarnings = FALSE)
  
  # now set your wd to that new - timestamped output folder 
  # setwd(paste(getwd(),"/",format(Sys.time(), "%m-%d-%Y-%H%M")," - simul",simulnumb,sep=""))
  # without the time stamp
  setwd(paste(getwd(),"/",parameters$simulation[simulnumb],sep=""))
  
  ###################################################
  # Plotting - option - use argument shapshots=TRUE #
  ###################################################
  if(shapshots==TRUE){
    
    require(raster)
    
    ###########
    # Initial #
    ###########   
    # make raster layers 
    LAND <- raster(LAND)
    SAV <- raster(LIST[[1]]$SAV)
    for (y in 1:numbFPspecies){
      assign(paste("FP0",y,sep=""),raster(LIST[[1]]$FP[[y]]))
    }
    TOTALN <- raster(LIST[[1]]$TOTALN/1000)
    
    # stack raster layers 
    # I need a smarter way to make this variable length 
    if (numbFPspecies == 4){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02,FP03,FP04)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02,FP03,FP04)
    }
    if (numbFPspecies == 3){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02,FP03)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02,FP03)
    }
    if (numbFPspecies == 2){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02)
    }
    if (numbFPspecies == 1){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01)
      all_layers <- stack(LAND,SAV,TOTALN,FP01)
    }
    
    # name raster layers 
    names(all_layers)[1] <- "LAND"
    names(all_layers)[2] <- "SAV"
    names(all_layers)[3] <- "TOTALN"
    for (y in 1:numbFPspecies){
      names(all_layers)[y+3] <- paste("FP0",y,sep="")
    }
    
    # save plot without the time stamp
    jpeg(file="initial.jpg",width=11,height=8,units="in",res=300)  
    plot(all_layers)
    dev.off()
    
    ############
    # Midpoint #
    ############
    # make raster layers 
    SAV <- raster(LIST[[timesteps/2]]$SAV)
    for (y in 1:numbFPspecies){
      assign(paste("FP0",y,sep=""),raster(LIST[[timesteps/2]]$FP[[y]]))
    }
    TOTALN <- raster(LIST[[timesteps/2]]$TOTALN/1000)
    
    # stack raster layers 
    # I need a smarter way to make this variable length 
    if (numbFPspecies == 4){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02,FP03,FP04)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02,FP03,FP04)
    }
    if (numbFPspecies == 3){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02,FP03)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02,FP03)
    }
    if (numbFPspecies == 2){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02)
    }
    if (numbFPspecies == 1){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01)
      all_layers <- stack(LAND,SAV,TOTALN,FP01)
    }
    
    # name raster layers 
    names(all_layers)[1] <- "LAND"
    names(all_layers)[2] <- "SAV"
    names(all_layers)[3] <- "TOTALN"
    for (y in 1:numbFPspecies){
      names(all_layers)[y+3] <- paste("FP0",y,sep="")
    }
    
    # save plot without the time stamp
    jpeg(file="middle.jpg",width=11,height=8,units="in",res=300)  
    plot(all_layers)
    dev.off()
    
    #########
    # Final #
    #########
    # make raster layers 
    SAV <- raster(LIST[[timesteps-1]]$SAV)
    for (y in 1:numbFPspecies){
      assign(paste("FP0",y,sep=""),raster(LIST[[timesteps-1]]$FP[[y]]))
    }
    TOTALN <- raster(LIST[[timesteps-1]]$TOTALN/1000)
    
    # stack raster layers 
    # I need a smarter way to make this variable length 
    if (numbFPspecies == 4){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02,FP03,FP04)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02,FP03,FP04)
    }
    if (numbFPspecies == 3){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02,FP03)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02,FP03)
    }
    if (numbFPspecies == 2){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01,FP02)
      all_layers <- stack(LAND,SAV,TOTALN,FP01,FP02)
    }
    if (numbFPspecies == 1){
      #all_layers <- stack(LAND,SAV,FPtotal,FP01)
      all_layers <- stack(LAND,SAV,TOTALN,FP01)
    }
    
    # name raster layers 
    names(all_layers)[1] <- "LAND"
    names(all_layers)[2] <- "SAV"
    names(all_layers)[3] <- "TOTALN"
    for (y in 1:numbFPspecies){
      names(all_layers)[y+3] <- paste("FP0",y,sep="")
    }
    
    # save plot without the time stamp
    jpeg(file="final.jpg",width=11,height=8,units="in",res=300)  
    plot(all_layers)
    dev.off()
  }
  
  #####################################################
  # Plotting - option - use argument timeseries=TRUE #
  #####################################################
  if(timeseries==TRUE){
    
    require(reshape2)    
    require(ggplot2)
    
    ########################
    # % cover through time #
    ########################
    # creates a blank data frame where this all will go 
    data_cover <- NULL
    data_cover <- as.data.frame(data_cover)  
    
    # for each timestep, for each species, assign the percent cover to the appropriate position in data_cover
    for (i in 1:(timesteps+1)) { 
      
      LIST[[i]]$SAV[LIST[[i]]$SAV > 100] <- 100
      data_cover[i,1] <- sum(LIST[[i]]$SAV)/area
      
      LIST[[i]]$FPtotal[LIST[[i]]$FPtotal > 100] <- 100
      data_cover[i,2] <- sum(LIST[[i]]$FPtotal)/area
      
      for (m in 1:length(LIST[[i]]$FP)){
        LIST[[i]]$FP[[m]][LIST[[i]]$FP[[m]] > 100] <- 100
        data_cover[i,m+2] <- sum(LIST[[i]]$FP[[m]])/area
      }
      
    } # ends for loop through time steps 
    
    # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
    names<-c("SAV","All_FP")
    for (n in 1:numbFPspecies){
      names <- append(names, paste("FP_0",n,sep="",collapse=""))
    }
    
    # assign that vector to the column names of your dataframe 
    names(data_cover) <- names
    
    # add time to your dataframe 
    time <- seq(1,timesteps+1,1)
    data_cover <- cbind(time,data_cover)
    
    # reshape your data 1st before trying ggplot2 
    data_cover_melt <- melt(data_cover,id.vars="time")
    
    temp <- ggplot(data_cover_melt, aes(x=time,y=value,colour=variable)) 
    temp <- temp + geom_line() 
    temp <- temp + ylab("percent cover")
    temp <- temp + theme_classic(base_size=18)
    
    # save plot without the time stamp
    ggsave(filename="cover.jpg",width=11,height=8,units="in")
    
    ###############################
    # Plot nutrients through time #
    ###############################
    # creates a blank data frame where this all will go 
    data_nutrients <- NULL
    data_nutrients <- as.data.frame(data_nutrients)
    
    # for each timestep, for each species, assign the nutrients to the appropriate position in data_nutrients
    for (i in 1:(timesteps+1)) { 
      data_nutrients[i,1] <- min(LIST[[i]]$TOTALN/1000)
      data_nutrients[i,2] <- sum(LIST[[i]]$TOTALN/1000)/area
      data_nutrients[i,3] <- max(LIST[[i]]$TOTALN/1000)
    }
    
    # add time to your dataframe 
    time <- seq(1,(timesteps+1),1)
    data_nutrients <- cbind(time,data_nutrients)
    
    # assign the correct names to the column names of your dataframe 
    names(data_nutrients) <- c("time",
                             "min_totalN","mean_totalN","max_totalN")
    
    # reshape your data 1st before trying ggplot2 
    data_nutrients_melt <- melt(data_nutrients,id.vars="time")
    
    # This is a stupid and ugly way of reshaping data 
    # goal is to have a column for the nutr and a column for the statistic 
    data_nutrients_melt$nutr[data_nutrients_melt$variable == "min_totalN"] <- "totalN"
    data_nutrients_melt$nutr[data_nutrients_melt$variable == "mean_totalN"] <- "totalN"
    data_nutrients_melt$nutr[data_nutrients_melt$variable == "max_totalN"] <- "totalN"
    
    data_nutrients_melt$stat[data_nutrients_melt$variable == "min_totalN"] <- "min"
    data_nutrients_melt$stat[data_nutrients_melt$variable == "mean_totalN"] <- "mean"
    data_nutrients_melt$stat[data_nutrients_melt$variable == "max_totalN"] <- "max"
    
    temp <- ggplot(data_nutrients_melt, aes(x=time,y=value,colour=nutr,linetype=stat)) 
    temp <- temp + geom_line()
    temp <- temp + scale_linetype_manual(values=c("dashed","solid","dashed"))
    temp <- temp + ylab("nutrient concentration(mg/L)")
    temp <- temp + theme_classic(base_size=18)
    
    # save plot without the time stamp
    ggsave(filename="nutrients.jpg",width=11,height=8,units="in")
  }
  
  ######################################### 
  # Statistics to output to the .csv file # 
  #########################################
  # INDEXING THE DAY BEFORE OVERWINTERING
  # days 
  # 2*days+1
  # 3*days+2
  # 4*days+3
  
  if (years == 1){
    # Make SAV biomass > 100 turn to 100 
    LIST[[days]]$SAV[LIST[[days]]$SAV > 100] <- 100

    
    # Make FP biomass > 100 turn to 100 
    for (m in 1:length(LIST[[1]]$FP)){
      LIST[[days]]$FP[[m]][LIST[[days]]$FP[[m]] > 100] <- 100
    }
    
    # Calculate %SAV cover 
    SAV_end_yr01 <- sum(LIST[[days]]$SAV)/area
    SAV_end_yr02 <- NA
    SAV_end_yr03 <- NA
    SAV_end_yr04 <- NA
    
    # Calculate %FP cover 
    for (m in 1:length(LIST[[1]]$FP)){
      FP_end_yr01 <- sum(LIST[[days]]$FP[[m]])/area
      FP_end_yr02 <- NA
      FP_end_yr03 <- NA
      FP_end_yr04 <- NA
    }
    
    # Calculate TOTAL N
    TOTALN_end_yr01 <- sum(LIST[[days]]$TOTALN/1000)/area
    TOTALN_end_yr02 <- NA
    TOTALN_end_yr03 <- NA
    TOTALN_end_yr04 <- NA
  }
  
  
  else {
    # Make SAV biomass > 100 turn to 100 
    LIST[[days]]$SAV[LIST[[days]]$SAV > 100] <- 100
    LIST[[2*days+1]]$SAV[LIST[[2*days+1]]$SAV > 100] <- 100
    LIST[[3*days+2]]$SAV[LIST[[3*days+2]]$SAV > 100] <- 100
    LIST[[4*days+3]]$SAV[LIST[[4*days+3]]$SAV > 100] <- 100
    
    # Make FP biomass > 100 turn to 100 
    for (m in 1:length(LIST[[1]]$FP)){
      LIST[[days]]$FP[[m]][LIST[[days]]$FP[[m]] > 100] <- 100
      LIST[[2*days+1]]$FP[[m]][LIST[[2*days+1]]$FP[[m]] > 100] <- 100
      LIST[[3*days+2]]$FP[[m]][LIST[[3*days+2]]$FP[[m]] > 100] <- 100
      LIST[[4*days+3]]$FP[[m]][LIST[[4*days+3]]$FP[[m]] > 100] <- 100
    }
    
    # Calculate %SAV cover 
    SAV_end_yr01 <- sum(LIST[[days]]$SAV)/area
    SAV_end_yr02 <- sum(LIST[[2*days+1]]$SAV)/area
    SAV_end_yr03 <- sum(LIST[[3*days+2]]$SAV)/area
    SAV_end_yr04 <- sum(LIST[[4*days+3]]$SAV)/area
    
    # Calculate %FP cover 
    for (m in 1:length(LIST[[1]]$FP)){
      FP_end_yr01 <- sum(LIST[[days]]$FP[[m]])/area
      FP_end_yr02 <- sum(LIST[[2*days+1]]$FP[[m]])/area
      FP_end_yr03 <- sum(LIST[[3*days+2]]$FP[[m]])/area
      FP_end_yr04 <- sum(LIST[[4*days+3]]$FP[[m]])/area
    }
    
    # Calculate TOTAL N
    TOTALN_end_yr01 <- sum(LIST[[days]]$TOTALN/1000)/area
    TOTALN_end_yr02 <- sum(LIST[[2*days+1]]$TOTALN/1000)/area
    TOTALN_end_yr03 <- sum(LIST[[3*days+2]]$TOTALN/1000)/area
    TOTALN_end_yr04 <- sum(LIST[[4*days+3]]$TOTALN/1000)/area
  } 
      
  # assign those variables to the environment outside of this function 
  assign("SAV_end_yr01", SAV_end_yr01, pos = 1)
  assign("SAV_end_yr02", SAV_end_yr02, pos = 1)
  assign("SAV_end_yr03", SAV_end_yr03, pos = 1)
  assign("SAV_end_yr04", SAV_end_yr04, pos = 1)
  assign("FP_end_yr01", FP_end_yr01, pos = 1)
  assign("FP_end_yr02", FP_end_yr02, pos = 1)
  assign("FP_end_yr03", FP_end_yr03, pos = 1)
  assign("FP_end_yr04", FP_end_yr04, pos = 1)
  assign("TOTALN_end_yr01", TOTALN_end_yr01, pos = 1)
  assign("TOTALN_end_yr02", TOTALN_end_yr02, pos = 1)
  assign("TOTALN_end_yr03", TOTALN_end_yr03, pos = 1)
  assign("TOTALN_end_yr04", TOTALN_end_yr04, pos = 1)
  
  
  #############################################
  # return to your original working directory #
  #############################################
  setwd(original_wd)
}