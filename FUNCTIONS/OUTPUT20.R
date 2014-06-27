#########################################################
# Outputs model results                                 #
# Replaces functions ANIMATE() and PLOT()               #
#                                                       #
# Compatible with new LIST structure                    #      
# Compatible with SAV component                         #
#                                                       # 
# By: Michael J. McCann                                 #
# Last Updated: 4/2014                                  #
######################################################### 

OUTPUT20 <- function(animate=FALSE,regimethreshold=70){  
  
  require(raster)
  
  # save your original working directory
  original_wd <- getwd()
  
  # then change the working directory for your output files to /OUTPUT 
  setwd(paste(getwd(),"/OUTPUT",sep=""))
  
  # create another folder to hold the animation results - inside the /OUTPUT folder 
  dir.create(paste(format(Sys.time(), "%m-%d-%Y-%H%M")," - simul",simulnumb, sep=""))
  
  # now set your wd to that new - timestamped output folder 
  setwd(paste(getwd(),"/",format(Sys.time(), "%m-%d-%Y-%H%M")," - simul",simulnumb,sep=""))
  
  ##############################################################
  ############## animated plotting - package animate ###########
  ##############################################################
  if (animate == TRUE){
    require(animation)
    
    saveHTML({
      
      ani.options(interval=0.2, nmax=(timesteps+1),verbose=FALSE)
      
      for(i in 1:(timesteps+1)) { # loop through time steps
        plot(raster(LIST[[i]]$FPtotal),main=paste("Floating Plants ","Timestep ",i,sep=""))
        ani.pause()
      }
    },
    outdir=getwd()
    )
    
    saveHTML({
      
      ani.options(interval=0.2, nmax=(timesteps+1),verbose=FALSE)
      
      for(i in 1:(timesteps+1)) { # loop through time steps
        plot(raster(LIST[[i]]$SAV),main=paste("Submerged plants ","Timestep ",i,sep=""))
             ani.pause()
      }
    },
    outdir=getwd()
    )
  }
  #########################################################################
  ################### generate a .gif of the simulation ###################
  #########################################################################
  require(caTools)
  
  jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                                  "yellow", "#FF7F00", "red", "#7F0000")) # define "jet" palette
  
  image = array(0, c(height, width, (timesteps+1)))
  
  for(i in 1:(timesteps+1)) image[,,i] = LIST[[i]]$FPtotal
  
  write.gif(image, filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," FP animate", ".gif", sep=""), 
            scale="always", col="jet.colors")  
  
  rm(image)
  
  image = array(0, c(height, width, (timesteps+1)))
  
  for(i in 1:(timesteps+1)) image[,,i] = LIST[[i]]$SAV
  
  write.gif(image, filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," SAV animate", ".gif", sep=""), 
            scale="always", col="jet.colors")  
  
  #########################################################################
  ################### save a few snapshots of the waterbody ###############
  #########################################################################
  require(raster)
  
  # new plotting: zlim should set the constant scale 
  jpeg(file=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," snapshots", ".jpg", sep=""),width=11,height=8,units="in",res=300)
  par(mfrow=c(3,2))
  plot(raster(LIST[[1]]$SAV),main="SAV Initial")
  plot(raster(LIST[[1]]$FPtotal),main="All FP species Initial")
  plot(raster(LIST[[timesteps/2]]$SAV),main="SAV Midpoint")
  plot(raster(LIST[[timesteps/2]]$FPtotal),main="All FP species Midpoint")
  plot(raster(LIST[[timesteps-1]]$SAV),main="SAV Final")
  plot(raster(LIST[[timesteps-1]]$FPtotal),main="All FP species Final")
  dev.off()
    
  #####################################################################################  
  ##### Plot average biomass (by cell) of all species through time - GGPLOT2 ##########
  #####################################################################################
  require(ggplot2)
  require(reshape2)
  
  # creates a blank data frame where this all will go 
  data_biomass_avg<-NULL
  data_biomass_avg<-as.data.frame(data_biomass_avg)
  
  # for each timestep, for each species, assign the popl. size to the appropriate position in data_biomass_avg
  
  
  for (i in 1:(timesteps+1)) { 
    data_biomass_avg[i,1]<-mean(LIST[[i]]$SAV)
    data_biomass_avg[i,2]<-mean(LIST[[i]]$FPtotal)
    for (j in 1:length(LIST[[i]]$FP)){
      data_biomass_avg[i,j+2]<-mean(LIST[[i]]$FP[[j]])
    }
  }
  
  # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
  names<-c("SAV","All_FP")
  for (n in 1:numbFPspecies){
    names<-append(names, paste("FP_0",n,sep="",collapse=""))
  }
  
  # assign that vector to the column names of your dataframe 
  names(data_biomass_avg)<-names
  
  # add time to your dataframe 
  time <- seq(1,timesteps+1,1)
  data_biomass_avg<-cbind(time,data_biomass_avg)
  
  #reshape your data 1st before trying ggplot2 
  data_biomass_avg_melt <- melt(data_biomass_avg,id.vars="time")
  
  ggplot(data_biomass_avg_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("average biomass (g/sq.mm)")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," average biomass", ".jpg", sep=""),width=11,height=8,units="in")
    
  #################################################################
  ############# Plot % cover through time - GGPLOT2 ###############
  #################################################################
  # creates a blank data frame where this all will go 
  data_cover<-NULL
  data_cover<-as.data.frame(data_cover)
  
  # for each timestep, for each species, assign the percent cover to the appropriate position in data_cover
  for (i in 1:(timesteps+1)) { 
    for (j in 1:height) { # loop over all rows (height)
      for (k in 1:width) { # loop over all columns (width) 
        
        if (LIST[[i]]$SAV[j,k] > 100) {LIST[[i]]$SAV[j,k] <- 100}
        data_cover[i,1]<-mean(LIST[[i]]$SAV)
        
        if (LIST[[i]]$FPtotal[j,k] > 100) {LIST[[i]]$FPtotal[j,k] <- 100}
        data_cover[i,2]<-mean(LIST[[i]]$FPtotal)
        
        for (m in 1:length(LIST[[i]]$FP)){
          if (LIST[[i]]$FP[[m]][j,k] > 100) {LIST[[i]]$FP[[m]][j,k] <- 100}
          data_cover[i,m+2]<-mean(LIST[[i]]$FP[[m]])      
        }
      }
    }
  } # ends for loop through time steps 
  
  # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
  names<-c("SAV","All_FP")
  for (n in 1:numbFPspecies){
    names<-append(names, paste("FP_0",n,sep="",collapse=""))
  }
  
  # assign that vector to the column names of your dataframe 
  names(data_cover)<-names
    
  # add time to your dataframe 
  time <- seq(1,timesteps+1,1)
  data_cover<-cbind(time,data_cover)
  
  # reshape your data 1st before trying ggplot2 
  data_cover_melt <- melt(data_cover,id.vars="time")
  
  ggplot(data_cover_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("percent cover")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," percent cover", ".jpg", sep=""),width=11,height=8,units="in")
  
  # dev.off()
  
  #################################################################
  ############# Plot nutrients through time - GGPLOT2 #############
  #################################################################
  # creates a blank data frame where this all will go 
  data_nutrients<-NULL
  data_nutrients<-as.data.frame(data_nutrients)
  
  # for each timestep, for each species, assign the percent cover to the appropriate position in data_nutrients
  for (i in 1:(timesteps+1)) { 
    data_nutrients[i,1]<-LIST[[i]]$TOTALN
    data_nutrients[i,2]<-LIST[[i]]$TOTALP
  }
    
  # add time to your dataframe 
  time <- seq(1,(timesteps+1),1)
  data_nutrients<-cbind(time,data_nutrients)
  
  # assign the correct names to the column names of your dataframe 
  names(data_nutrients)<-c("time","Total N","Total P")
  
  # reshape your data 1st before trying ggplot2 
  data_nutrients_melt <- melt(data_nutrients,id.vars="time")
  
  ggplot(data_nutrients_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("nutrient concentration(mg/L)")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," nutrients", ".jpg", sep=""),width=11,height=8,units="in")
  
  # dev.off()
  
  #################################################################
  ############# Plot % cells occupied through time - GGPLOT2 ######
  #################################################################
  # creates a blank data frame where this all will go 
  data_cell_occupancy <-NULL
  data_cell_occupancy<-as.data.frame(data_cell_occupancy)
  
  # for each timestep, count up how many cells are occupied (at any biomass/cover level)
  for (i in 1:(timesteps+1)) { 
    data_cell_occupancy[i,1] <- length(LIST[[i]]$SAV[LIST[[i]]$SAV > 0])
    data_cell_occupancy[i,2] <- length(LIST[[i]]$FPtotal[LIST[[i]]$FPtotal > 0])
    data_cell_occupancy[i,3] <- (length(LIST[[i]]$SAV[LIST[[i]]$SAV > 0]) / (height*width)) * 100
    data_cell_occupancy[i,4] <- (length(LIST[[i]]$FPtotal[LIST[[i]]$FPtotal > 0]) / (height*width)) * 100
  }
    
  # add time to your dataframe 
  time <- seq(1,(timesteps+1),1)
  data_cell_occupancy <- cbind(time,data_cell_occupancy)
  
  # generate a vector for naming your columns 
  names<-c("time","# cells occupied - SAV","# cells occupied - FP","% cells occupied - SAV","% cells occupied - FP")
  
  # assign that vector to the column names of your dataframe 
  names(data_cell_occupancy)<-names
  
  # reshape your data 1st before trying ggplot2 
  data_cell_occupancy_melt <- melt(data_cell_occupancy,id.vars="time")
  
  # Change this back to the command two lines down if it does not work 
  # ggplot(data_cell_occupancy_melt, aes(x=time,y=perc_cells_occup,colour=variable)) + geom_line() + ylab("cells occupied")
  ggplot(data_cell_occupancy_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("cells occupied")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," cells occupied", ".jpg", sep=""),width=11,height=8,units="in")
  
  # dev.off()
    
  ####################################################
  ########## Summary statistics - by year ############
  ####################################################
  # Set up data frames to hold summary statistics 
  # make a vector of "year"
  year <- NULL
  for (i in 1:years){year <- append(year,rep(i,days+1))}
  year <- append(year, i+1) # this is for the first day of the next year 

  data_cover$year <- year # add year to your dataframe 
  data_cover$day <- c(rep(seq(1,days+1),years),1)  # add a vector of "day" instead of "time" to the data frame 
   
  data_biomass_avg$year <- year # add year to your dataframe 
  data_biomass_avg$day <- c(rep(seq(1,days+1),years),1)  # add a vector of "day" instead of "time" to the data frame 
  
  ######
  # FP #
  ######
  # ***Average*** floating plant ***cover*** for all time steps in each year 
  avgFPcover <- aggregate(data_cover$All_FP,list(year=data_cover$year),mean) 
  colnames(avgFPcover)[2] <- "avgFPcover"
  
  # ***Maximum*** floating plant ***cover*** for all time steps in each year 
  maxFPcover <- aggregate(data_cover$All_FP,list(year=data_cover$year),max) 
  colnames(maxFPcover)[2] <- "maxFPcover"
  
  # ***Average*** average floating plant ***biomass*** by cell for all time steps in each year 
  avgavgFPbiomass <- aggregate(data_biomass_avg$All_FP,list(year=data_biomass_avg$year),mean) 
  colnames(avgavgFPbiomass)[2] <- "avgavgFPbiomass"
  
  #######
  # SAV #
  ####### 
  # ***Average*** submerged plant ***cover*** for all time steps in each year 
  avgSAVcover <- aggregate(data_cover$SAV,list(year=data_cover$year),mean) 
  colnames(avgSAVcover)[2] <- "avgSAVcover"
  
  # ***Maximum*** submerged plant ***cover*** for all time steps in each year 
  maxSAVcover <- aggregate(data_cover$SAV,list(year=data_cover$year),max) 
  colnames(maxSAVcover)[2] <- "maxSAVcover"
  
  # ***Average*** average floating plant ***biomass*** by cell for all time steps in each year 
  avgavgSAVbiomass <- aggregate(data_biomass_avg$SAV,list(year=data_biomass_avg$year),mean) 
  colnames(avgavgSAVbiomass)[2] <- "avgavgSAVbiomass"
  
  ######
  # FP #
  ######
  # ***Number*** of days each year that the waterbody is above a treshold value of All_FP
  apply.fun <- function(x) {
    sum(x > regimethreshold)
  }
  numb_daysFP <- aggregate(data_cover$All_FP,list(year=data_cover$year),apply.fun) 
  colnames(numb_daysFP)[2] <- "numb_daysFP"
  
  # ***Proportion*** of days each year that the waterbody is above a treshold value of All_FP
  apply.fun <- function(x) {
    sum((x > regimethreshold)/days)
  }
  prop_daysFP <- aggregate(data_cover$All_FP,list(year=data_cover$year),apply.fun) 
  colnames(prop_daysFP)[2] <- "prop_daysFP"
    
  # ***First day*** that the waterbody is above a threshold value of All_FP 
  firstdayFP <- seq(from=0,to=0,length=years)
  for (j in 1:years) { # loop by years 
    if (prop_daysFP$prop_daysFP[j] > 0) {
      temp <- subset(data_cover$day,data_cover$year == j & data_cover$All_FP >= regimethreshold) # need to fix for NAs # need to fix so it returns day instead of index 
      firstdayFP[j] <- min(temp)
    }
    else (firstdayFP[j] <- NA)
  }
  # firstdayFP[is.infinite(firstdayFP)] <- NA # min() returns infinity if there are no numbers, so replaces Inf with NA
  firstdayFP[j+1] <- NA # add an NA for the first day of the last year (years+1)
  
  # this is ugly, but i need this to be in a dataframe with years and firstdayFP
  firstdayFP <- cbind(avgavgFPbiomass[,-2],firstdayFP)
  colnames(firstdayFP)[1] <- "year"
  
  #######
  # SAV #
  #######
  # ***Number*** of days each year that the waterbody is above a treshold value of SAV
  apply.fun <- function(x) {
    sum(x > regimethreshold)
  }
  numb_daysSAV <- aggregate(data_cover$SAV,list(year=data_cover$year),apply.fun) 
  colnames(numb_daysSAV)[2] <- "numb_daysSAV"
  
  # ***Proportion*** of days each year that the waterbody is above a treshold value of SAV
  apply.fun <- function(x) {
    sum((x > regimethreshold)/days)
  }
  prop_daysSAV <- aggregate(data_cover$SAV,list(year=data_cover$year),apply.fun) 
  colnames(prop_daysSAV)[2] <- "prop_daysSAV"
  
  # ***First day*** that the waterbody is above a threshold value of SAV 
  firstdaySAV <- seq(from=0,to=0,length=years)
  for (j in 1:years) { # loop by years 
    if (prop_daysSAV$prop_daysSAV[j] > 0) {
      temp <- subset(data_cover$day,data_cover$year == j & data_cover$SAV >= regimethreshold) # need to fix for NAs # need to fix so it returns day instead of index 
      firstdaySAV[j] <- min(temp)
    }
    else (firstdaySAV[j] <- NA)
  }
  # firstdaySAV[is.infinite(firstdaySAV)] <- NA # min() returns infinity if there are no numbers, so replaces Inf with NA
  firstdaySAV[j+1] <- NA # add an NA for the first day of the last year (years+1)
  
  # this is ugly, but i need this to be in a dataframe with years and firstdaySAV
  firstdaySAV <- cbind(avgavgSAVbiomass[,-2],firstdaySAV)
  colnames(firstdaySAV)[1] <- "year"
  
  
  # Build a data frame with all of these different summary statistics for each year 
  # This data frame will be used to get summary statistics across years 
  data_summary_by_year <- merge(avgFPcover,
                                merge(maxFPcover,
                                      merge(avgSAVcover,
                                            merge(maxSAVcover,
                                                merge(numb_daysFP,
                                                    merge(prop_daysFP,
                                                        merge(firstdayFP,
                                                              merge(numb_daysSAV,
                                                                    merge(prop_daysSAV,
                                                                        merge(firstdaySAV,
                                                                          merge(avgavgFPbiomass,avgavgSAVbiomass,
                                                                              )))))))))))
    
  # save it 
  write.csv(data_summary_by_year,file=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," results summary", ".csv", sep=""),row.names=F)
  

  #########################################################
  ########## Summary statistics - across years ############
  #########################################################
  ###### 
  # FP #
  ###### 
  # create variables 
  # prop years with avgFPcover > regimethreshold
  propyears_avgFPcover_abovethreshold <- sum(data_summary_by_year$avgFPcover >= regimethreshold)/years
  
  # prop years with prop_daysFP > 0.5
  propyears_prop_daysFP_abovehalf <- sum(data_summary_by_year$prop_daysFP >= 0.5)/years
  
  # average FP cover across years (excluding 1st three)
  avg_avg_FPcover <- mean(data_summary_by_year$avgFPcover[4:years])
  
  # average maximum FP cover across years (excluding 1st three)
  avg_max_FPcover <- mean(data_summary_by_year$maxFPcover[4:years])
  
  # average firstdayFP cover across years (excluding 1st three)
  avg_firstdayFP <- mean(data_summary_by_year$firstdayFP[4:years])
  
  # assign those variables to the environment outside of this function 
  assign("propyears_avgFPcover_abovethreshold", propyears_avgFPcover_abovethreshold, pos = 1)
  assign("propyears_prop_daysFP_abovehalf", propyears_prop_daysFP_abovehalf, pos = 1)
  assign("avg_avg_FPcover", avg_avg_FPcover, pos = 1)
  assign("avg_max_FPcover", avg_max_FPcover, pos = 1)
  assign("avg_firstdayFP", avg_firstdayFP, pos = 1)
  
  ####### 
  # SAV #
  ####### 
  # create variables 
  # prop years with avgFPcover > regimethreshold
  propyears_avgSAVcover_abovethreshold <- sum(data_summary_by_year$avgSAVcover >= regimethreshold)/years
  
  # prop years with prop_daysSAV > 0.5
  propyears_prop_daysSAV_abovehalf <- sum(data_summary_by_year$prop_daysSAV >= 0.5)/years
  
  # average SAV cover across years (excluding 1st three)
  avg_avg_SAVcover <- mean(data_summary_by_year$avgSAVcover[4:years])

  # average SAV cover across years (excluding 1st three)
  avg_max_SAVcover <- mean(data_summary_by_year$maxSAVcover[4:years])
  
  # average firstdaySAV across years (excluding 1st three)
  avg_firstdaySAV <- mean(data_summary_by_year$firstdaySAV[4:years])
  
  # assign those variables to the environment outside of this function 
  assign("propyears_avgSAVcover_abovethreshold", propyears_avgSAVcover_abovethreshold, pos = 1)
  assign("propyears_prop_daysSAV_abovehalf", propyears_prop_daysSAV_abovehalf, pos = 1)
  assign("avg_avg_SAVcover", avg_avg_SAVcover, pos = 1)
  assign("avg_max_SAVcover", avg_max_SAVcover, pos = 1)
  assign("avg_firstdaySAV", avg_firstdaySAV, pos = 1)
  
  ########################################################################
  ########### write a .txt of ALL parameter values in workspace ##########
  ########################################################################
  sink(file=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," parameters", ".txt", sep=""))
  print(ls.str(pos = 1),max.level=0)
  print(speciesmatrix)
  sink()
  
  ########################################################################
  ############# write a .txt of just speciesmatrix values ################
  ########################################################################
  sink(file=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," speciesmatrix", ".txt", sep=""))
  print(speciesmatrix)
  sink()
  
  # return to your original working directory
  setwd(original_wd)
}