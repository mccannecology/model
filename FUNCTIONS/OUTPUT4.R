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

OUTPUT4 <- function(animate=FALSE,threshold){  
  
  # assign the regimthreshold input value to the working environment for this function 
  regimethreshold <- threshold
  
  require(animation)
  require(raster)
  
  # save your original working directory
  original_wd <- getwd()
  
  # then change the working directory for your output files to /OUTPUT 
  setwd(paste(getwd(),"/OUTPUT",sep=""))
  
  # create another folder to hold the animation results - inside the /OUTPUT folder 
  dir.create(paste(format(Sys.time(), "%m-%d-%Y-%H%M")," - simul",simulnumb, sep=""))
  
  # now set your wd to that new - timestamped output folder 
  setwd(paste(getwd(),"/",format(Sys.time(), "%m-%d-%Y-%H%M")," - simul",simulnumb,sep=""))
  
  ###########################################################################
  ################ now do the animated plotting - package animate ###########
  ###########################################################################
  if (animate == TRUE){
    saveHTML({
      
      ani.options(interval=0.2, nmax=(1+(timesteps+1)*years),verbose=FALSE)
      
      for(i in 1:(1+(timesteps+1)*years)) { # loop through time steps
        plot(raster(LIST[[i]]$FPALLmatrix),main=paste("Floating Plants ","Timestep ",i,sep=""))
        ani.pause()
      }
    },
    outdir=getwd()
    )
    
    saveHTML({
      
      ani.options(interval=0.2, nmax=(1+(timesteps+1)*years),verbose=FALSE)
      
      for(i in 1:(1+(timesteps+1)*years)) { # loop through time steps
        plot(raster(LIST[[i]]$SAVmatrix),main=paste("Submerged plants ","Timestep ",i,sep=""))
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
  
  image = array(0, c(height, width, (1+(timesteps+1)*years)))
  
  for(i in 1:(1+(timesteps+1)*years)) image[,,i] = LIST[[i]]$FPALLmatrix
  
  write.gif(image, filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," FP animate", ".gif", sep=""), 
            scale="always", col="jet.colors")  
  
  rm(image)
  
  image = array(0, c(height, width, (1+(timesteps+1)*years)))
  
  for(i in 1:(1+(timesteps+1)*years)) image[,,i] = LIST[[i]]$SAVmatrix
  
  write.gif(image, filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," SAV animate", ".gif", sep=""), 
            scale="always", col="jet.colors")  
  
  #####################################################################################  
  ############### Plot biomass of all species through time - GGPLOT2 ##################
  #####################################################################################
  require(ggplot2)
  require(reshape2)
  
  # creates a blank data frame where this all will go 
  data_biomass<-NULL
  data_biomass<-as.data.frame(data_biomass)
  
  # for each timestep, for each species, assign the popl. size to the appropriate position in data_biomass
  
  ### modify here with a if speciesnumb == 4
  
  for (i in 1:(1+(timesteps+1)*years)) { 
    if (numbFPspecies == 4) { 
      data_biomass[i,1]<-sum(LIST[[i]]$SAVmatrix)
      data_biomass[i,2]<-sum(LIST[[i]]$FPALLmatrix)
      data_biomass[i,3]<-sum(LIST[[i]]$FP1matrix)
      data_biomass[i,4]<-sum(LIST[[i]]$FP2matrix)
      data_biomass[i,5]<-sum(LIST[[i]]$FP3matrix)
      data_biomass[i,6]<-sum(LIST[[i]]$FP4matrix)
    }
    else if (numbFPspecies == 3) {
      data_biomass[i,1]<-sum(LIST[[i]]$SAVmatrix)
      data_biomass[i,2]<-sum(LIST[[i]]$FPALLmatrix)
      data_biomass[i,3]<-sum(LIST[[i]]$FP1matrix)
      data_biomass[i,4]<-sum(LIST[[i]]$FP2matrix)
      data_biomass[i,5]<-sum(LIST[[i]]$FP3matrix)
    }
    else if (numbFPspecies == 2) {
      data_biomass[i,1]<-sum(LIST[[i]]$SAVmatrix)
      data_biomass[i,2]<-sum(LIST[[i]]$FPALLmatrix)
      data_biomass[i,3]<-sum(LIST[[i]]$FP1matrix)
      data_biomass[i,4]<-sum(LIST[[i]]$FP2matrix)
    }
    else if (numbFPspecies == 1) {
      data_biomass[i,1]<-sum(LIST[[i]]$SAVmatrix)
      data_biomass[i,2]<-sum(LIST[[i]]$FPALLmatrix)
      data_biomass[i,3]<-sum(LIST[[i]]$FP1matrix)
    }
  }
  
  # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
  names<-c("SAV","All_FP")
  for (n in 1:numbFPspecies){
    names<-append(names, paste("FP_0",n,sep="",collapse=""))
  }
  
  # assign that vector to the column names of your dataframe 
  names(data_biomass)<-names
  
  # add time to your dataframe 
  data_biomass$time <- seq(1,1+(timesteps+1)*years,1)
  
  #reshape your data 1st before trying ggplot2 
  data_biomass_melt <- melt(data_biomass,id.vars="time")
  
  ggplot(data_biomass_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("total biomass (g)")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," biomass", ".jpg", sep=""))
  
  dev.off()
  
  #################################################################
  ############# Plot % cover through time - GGPLOT2 ###############
  #################################################################
  # creates a blank data frame where this all will go 
  data_cover<-NULL
  data_cover<-as.data.frame(data_cover)
  
  # for each timestep, for each species, assign the percent cover to the appropriate position in data_cover
  for (i in 1:(1+(timesteps+1)*years)) { 
    if (numbFPspecies == 4) { 
      data_cover[i,1]<-mean(LIST[[i]]$SAVmatrix)
      data_cover[i,2]<-mean(LIST[[i]]$FPALLmatrix)
      data_cover[i,3]<-mean(LIST[[i]]$FP1matrix)
      data_cover[i,4]<-mean(LIST[[i]]$FP2matrix)
      data_cover[i,5]<-mean(LIST[[i]]$FP3matrix)
      data_cover[i,6]<-mean(LIST[[i]]$FP4matrix)
    }
    else if (numbFPspecies == 3) {
      data_cover[i,1]<-mean(LIST[[i]]$SAVmatrix)
      data_cover[i,2]<-mean(LIST[[i]]$FPALLmatrix)
      data_cover[i,3]<-mean(LIST[[i]]$FP1matrix)
      data_cover[i,4]<-mean(LIST[[i]]$FP2matrix)
      data_cover[i,5]<-mean(LIST[[i]]$FP3matrix)
    }
    else if (numbFPspecies == 2) {
      data_cover[i,1]<-mean(LIST[[i]]$SAVmatrix)
      data_cover[i,2]<-mean(LIST[[i]]$FPALLmatrix)
      data_cover[i,3]<-mean(LIST[[i]]$FP1matrix)
      data_cover[i,4]<-mean(LIST[[i]]$FP2matrix)
    }
    else if (numbFPspecies == 1) {
      data_cover[i,1]<-mean(LIST[[i]]$SAVmatrix)
      data_cover[i,2]<-mean(LIST[[i]]$FPALLmatrix)
      data_cover[i,3]<-mean(LIST[[i]]$FP1matrix)
    }
  }
  
  # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
  names<-c("SAV","All_FP")
  for (n in 1:numbFPspecies){
    names<-append(names, paste("FP_0",n,sep="",collapse=""))
  }
  
  # assign that vector to the column names of your dataframe 
  names(data_cover)<-names
    
  # add time to your dataframe 
  data_cover$time <- seq(1,1+(timesteps+1)*years,1)
  
  # reshape your data 1st before trying ggplot2 
  data_cover_melt <- melt(data_cover,id.vars="time")
  
  ggplot(data_cover_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("percent cover")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," percent cover", ".jpg", sep=""))
  
  dev.off()
  
  #################################################################
  ############# Plot nutrients through time - GGPLOT2 #############
  #################################################################
  # creates a blank data frame where this all will go 
  data_nutrients<-NULL
  data_nutrients<-as.data.frame(data_nutrients)
  
  # for each timestep, for each species, assign the percent cover to the appropriate position in data_nutrients
  for (i in 1:(1+(timesteps+1)*years)) { 
    data_nutrients[i,1]<-LIST[[i]]$TOTALN
    data_nutrients[i,2]<-LIST[[i]]$TOTALP
  }
  
  # assign the correct names to the column names of your dataframe 
  names(data_nutrients)<-c("TOTAL_N","TOTAL_P")
  
  # add time to your dataframe 
  data_nutrients$time <- seq(1,1+(timesteps+1)*years,1)
  
  # reshape your data 1st before trying ggplot2 
  data_nutrients_melt <- melt(data_nutrients,id.vars="time")
  
  ggplot(data_nutrients_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("concentration(mg/L)")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," nutrients", ".jpg", sep=""))
  
  dev.off()
  
  #################################################################
  ############# Plot % cells occupied through time - GGPLOT2 ######
  #################################################################
  # creates a blank data frame where this all will go 
  data_cell_occupancy <-NULL
  data_cell_occupancy<-as.data.frame(data_cell_occupancy)
  
  # for each timestep, count up how many cells are occupied (at any biomass/cover level)
  for (i in 1:(1+(timesteps+1)*years)) { 
    data_cell_occupancy[i,1] <- length(LIST[[i]]$SAVmatrix[LIST[[i]]$SAVmatrix > 0])
    data_cell_occupancy[i,2] <- length(LIST[[i]]$FPALLmatrix[LIST[[i]]$FPALLmatrix > 0])
    data_cell_occupancy[i,3] <- (length(LIST[[i]]$SAVmatrix[LIST[[i]]$SAVmatrix > 0]) / (height*width)) * 100
    data_cell_occupancy[i,4] <- (length(LIST[[i]]$FPALLmatrix[LIST[[i]]$FPALLmatrix > 0]) / (height*width)) * 100
  }
  
  # generate a vector for naming your columns 
  names<-c("numb_cells_occup_SAV","numb_cells_occup_FP","perc_cells_occup_SAV","perc_cells_occup_FP")
  
  # assign that vector to the column names of your dataframe 
  names(data_cell_occupancy)<-names
  
  # add time to your dataframe 
  data_cell_occupancy$time <- seq(1,1+(timesteps+1)*years,1)
  
  # reshape your data 1st before trying ggplot2 
  data_cell_occupancy_melt <- melt(data_cell_occupancy,id.vars="time")
  
  # Change this back to the command two lines down if it does not work 
  # ggplot(data_cell_occupancy_melt, aes(x=time,y=perc_cells_occup,colour=variable)) + geom_line() + ylab("cells occupied")
  ggplot(data_cell_occupancy_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("cells occupied")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," cells occupied", ".jpg", sep=""))
  
  dev.off()
    
  #################################################################
  ########## Summary statistics of FP regime - by year ############
  #################################################################
  # make a vector of "year"
  year <- NULL
  for (i in 1:years){year <- append(year,rep(i,timesteps+1))}
  year <- append(year, i+1) # this is for the first day of the next year 

  data_cover$year <- year # add year to your dataframe 
  data_cover$day <- c(rep(seq(1,timesteps+1),years),1)  # add a vector of "day" instead of "time" to the data frame 
  
  data_biomass$year <- year # add year to your dataframe 
  data_biomass$day <- c(rep(seq(1,timesteps+1),years),1)  # add a vector of "day" instead of "time" to the data frame 
 
  # ***Average*** floating plant ***cover*** for all time steps in each year 
  avgFPcover <- aggregate(data_cover$All_FP,list(year=data_cover$year),mean) 
  colnames(avgFPcover)[2] <- "avgFPcover"
  
  # ***Maximum*** floating plant ***cover*** for all time steps in each year 
  maxFPcover <- aggregate(data_cover$All_FP,list(year=data_cover$year),max) 
  colnames(maxFPcover)[2] <- "maxFPcover"
  
  # ***Average*** floating plant ***biomass*** for all time steps in each year 
  avgFPbiomass <- aggregate(data_biomass$All_FP,list(year=data_biomass$year),mean) 
  colnames(avgFPbiomass)[2] <- "avgFPbiomass"
  

  
  # ***Average*** submerged plant ***cover*** for all time steps in each year 
  avgSAVcover <- aggregate(data_cover$SAV,list(year=data_cover$year),mean) 
  colnames(avgSAVcover)[2] <- "avgSAVcover"
  
  # ***Maximum*** submerged plant ***cover*** for all time steps in each year 
  maxSAVcover <- aggregate(data_cover$SAV,list(year=data_cover$year),max) 
  colnames(maxSAVcover)[2] <- "maxSAVcover"
  
  # ***Average*** submerged plant ***biomass*** for all time steps in each year 
  avgSAVbiomass <- aggregate(data_biomass$SAV,list(year=data_biomass$year),mean) 
  colnames(avgSAVbiomass)[2] <- "avgSAVbiomass"
  
  
  
  
  # ***Number*** of days each year that the waterbody is above a treshold value of All_FP
  apply.fun <- function(x) {
    sum(x > regimethreshold)
  }
  numb_daysFP <- aggregate(data_cover$All_FP,list(year=data_cover$year),apply.fun) 
  colnames(numb_daysFP)[2] <- "numb_daysFP"
  
  # ***Proportion*** of days each year that the waterbody is above a treshold value of All_FP
  apply.fun <- function(x) {
    sum((x > regimethreshold)/timesteps)
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
  firstdayFP <- cbind(avgFPbiomass[,-2],firstdayFP)
  colnames(firstdayFP)[1] <- "year"
  
  
  
  # ***Number*** of days each year that the waterbody is above a treshold value of SAV
  apply.fun <- function(x) {
    sum(x > regimethreshold)
  }
  numb_daysSAV <- aggregate(data_cover$SAV,list(year=data_cover$year),apply.fun) 
  colnames(numb_daysSAV)[2] <- "numb_daysSAV"
  
  # ***Proportion*** of days each year that the waterbody is above a treshold value of SAV
  apply.fun <- function(x) {
    sum((x > regimethreshold)/timesteps)
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
  firstdaySAV <- cbind(avgSAVbiomass[,-2],firstdaySAV)
  colnames(firstdaySAV)[1] <- "year"
  
  
  
  
  
  
  
  # Build a data frame with all of these different summary statistics for each year 
  data_summary_by_year <- merge(avgFPcover,
                                merge(maxFPcover,
                                      merge(avgSAVcover,
                                            merge(maxSAVcover,
                                                  merge(avgFPbiomass,
                                                        merge(avgSAVbiomass,
                                                            merge(numb_daysFP,
                                                                merge(prop_daysFP,
                                                                    merge(firstdayFP,
                                                                          merge(numb_daysSAV,
                                                                                merge(prop_daysSAV,firstdaySAV,
                                                                                          )))))))))))
    
  # assign it to something useful otuside of the function 
  write.csv(data_summary_by_year,file=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," results summary", ".csv", sep=""),row.names=F)
  

  
  
  # prop years with avgFPcover > regimethreshold
  propyears_avgFPcover_abovethreshold <- sum(data_summary_by_year$avgFPcover >= regimethreshold)/years
  
  # prop years with prop_daysFP > 0.5
  propyears_prop_daysFP_abovehalf <- sum(data_summary_by_year$prop_daysFP >= 0.5)/years
  
  # average FP cover across years (excluding 1st three)
  avg_avg_FPcover <- mean(data_summary_by_year$avgFPcover[4:years])
  
  assign("propyears_avgFPcover_abovethreshold", propyears_avgFPcover_abovethreshold, pos = 1)

  assign("propyears_prop_daysFP_abovehalf", propyears_prop_daysFP_abovehalf, pos = 1)

  assign("avg_avg_FPcover", avg_avg_FPcover, pos = 1)
  
  
  
  # prop years with avgFPcover > regimethreshold
  propyears_avgSAVcover_abovethreshold <- sum(data_summary_by_year$avgSAVcover >= regimethreshold)/years
  
  # prop years with prop_daysSAV > 0.5
  propyears_prop_daysSAV_abovehalf <- sum(data_summary_by_year$prop_daysSAV >= 0.5)/years
  
  # average SAV cover across years (excluding 1st three)
  avg_avg_SAVcover <- mean(data_summary_by_year$avgSAVcover[4:years])
  
  assign("propyears_avgSAVcover_abovethreshold", propyears_avgSAVcover_abovethreshold, pos = 1)
  
  assign("propyears_prop_daysSAV_abovehalf", propyears_prop_daysSAV_abovehalf, pos = 1)
  
  assign("avg_avg_SAVcover", avg_avg_SAVcover, pos = 1)
  
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