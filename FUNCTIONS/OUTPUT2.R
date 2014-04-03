#########################################################
# Outputs model results                                 #
# Replaces functions ANIMATE() and PLOT()               #
#                                                       #
# By: Michael J. McCann                                 #
# Last Updated: 2/26/2014                               #
######################################################### 

OUTPUT2 <- function(animate=FALSE, regimethreshold){  
  
  # assign the regimthreshold input value to the working environment for this function 
  regimethreshold <- regimethreshold 
  
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
        plot(raster(LIST[[i]]$SPALLmatrix),main=i)
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
  
  for(i in 1:(1+(timesteps+1)*years)) image[,,i] = LIST[[i]]$SPALLmatrix
  
  write.gif(image, filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," animate", ".gif", sep=""), 
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
    if (numbspecies == 4) { 
      data_biomass[i,1]<-sum(LIST[[i]]$SPALLmatrix)
      data_biomass[i,2]<-sum(LIST[[i]]$SP1matrix)
      data_biomass[i,3]<-sum(LIST[[i]]$SP2matrix)
      data_biomass[i,4]<-sum(LIST[[i]]$SP3matrix)
      data_biomass[i,5]<-sum(LIST[[i]]$SP4matrix)
    }
    else if (numbspecies == 3) {
      data_biomass[i,1]<-sum(LIST[[i]]$SPALLmatrix)
      data_biomass[i,2]<-sum(LIST[[i]]$SP1matrix)
      data_biomass[i,3]<-sum(LIST[[i]]$SP2matrix)
      data_biomass[i,4]<-sum(LIST[[i]]$SP3matrix)
    }
    else if (numbspecies == 2) {
      data_biomass[i,1]<-sum(LIST[[i]]$SPALLmatrix)
      data_biomass[i,2]<-sum(LIST[[i]]$SP1matrix)
      data_biomass[i,3]<-sum(LIST[[i]]$SP2matrix)
    }
    else if (numbspecies == 1) {
      data_biomass[i,1]<-sum(LIST[[i]]$SPALLmatrix)
      data_biomass[i,2]<-sum(LIST[[i]]$SP1matrix)
    }
  }
  
  # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
  names<-"all species"
  for (n in 1:numbspecies){
    names<-append(names, paste("species",n,sep="",collapse=""))
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
    if (numbspecies == 4) { 
      data_cover[i,1]<-mean(LIST[[i]]$SPALLmatrix)
      data_cover[i,2]<-mean(LIST[[i]]$SP1matrix)
      data_cover[i,3]<-mean(LIST[[i]]$SP2matrix)
      data_cover[i,4]<-mean(LIST[[i]]$SP3matrix)
      data_cover[i,5]<-mean(LIST[[i]]$SP4matrix)
    }
    else if (numbspecies == 3) {
      data_cover[i,1]<-mean(LIST[[i]]$SPALLmatrix)
      data_cover[i,2]<-mean(LIST[[i]]$SP1matrix)
      data_cover[i,3]<-mean(LIST[[i]]$SP2matrix)
      data_cover[i,4]<-mean(LIST[[i]]$SP3matrix)
    }
    else if (numbspecies == 2) {
      data_cover[i,1]<-mean(LIST[[i]]$SPALLmatrix)
      data_cover[i,2]<-mean(LIST[[i]]$SP1matrix)
      data_cover[i,3]<-mean(LIST[[i]]$SP2matrix)
    }
    else if (numbspecies == 1) {
      data_cover[i,1]<-mean(LIST[[i]]$SPALLmatrix)
      data_cover[i,2]<-mean(LIST[[i]]$SP1matrix)
    }
  }
  
  # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
  names<-"cover_ALL"
  for (n in 1:numbspecies){
    names<-append(names, paste("cover_sp",n,sep="",collapse=""))
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
  names(data_nutrients)<-c("Total N","TOTAL P")
  
  # add time to your dataframe 
  data_nutrients$time <- seq(1,1+(timesteps+1)*years,1)
  
  # reshape your data 1st before trying ggplot2 
  data_nutrients_melt <- melt(data_nutrients,id.vars="time")
  
  ggplot(data_nutrients_melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("concentration(mg/L)")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," nutrients", ".jpg", sep=""))
  
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
  
  # ***Average*** floating plant ***cover*** for all time steps in each year 
  avgFP <- aggregate(data_cover$cover_ALL,list(year=data_cover$year),mean) 
  colnames(avgFP)[2] <- "avgFP"
  
  # ***Number*** of days each year that the waterbody is above a treshold value of cover_ALL
  apply.fun <- function(x) {
    sum(x > regimethreshold)
  }
  daysFP <- aggregate(data_cover$cover_ALL,list(year=data_cover$year),apply.fun) 
  colnames(daysFP)[2] <- "daysFP"
  
  # ***Proportion*** of days each year that the waterbody is above a treshold value of cover_ALL
  apply.fun <- function(x) {
    sum(x > regimethreshold)/timesteps
  }
  propdaysFP <- aggregate(data_cover$cover_ALL,list(year=data_cover$year),apply.fun) 
  colnames(propdaysFP)[2] <- "propdaysFP"
    
  # ***First day*** that the waterbody is above a threshold value of cover_ALL 
  firstdayFP <- seq(from=0,to=0,length=years)
  for (j in 1:years) { # loop by years 
    if (propdaysFP$propdaysFP[j] > 0) {
      temp <- subset(data_cover$day,data_cover$year == j & data_cover$cover_ALL >= regimethreshold) # need to fix for NAs # need to fix so it returns day instead of index 
      firstdayFP[j] <- min(temp)
    }
    else (firstdayFP[j] <- NA)
  }
  # firstdayFP[is.infinite(firstdayFP)] <- NA # min() returns infinity if there are no numbers, so replaces Inf with NA
  firstdayFP[j+1] <- NA # add an NA for the first day of the last year (years+1)
  
  # Build a data frame with all of these different summary statistics for each year 
  # I can probably do this smarter than just repeated merge()
  data_summary_by_year <- merge(avgFP,daysFP)
  data_summary_by_year <- merge(data_summary_by_year, propdaysFP)
  data_summary_by_year <- cbind(data_summary_by_year,firstdayFP)
  data_summary_by_year
  
  # assign it to something useful otuside of the function 
  write.csv(data_summary_by_year,file=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," results summary", ".csv", sep=""),row.names=F)
  
  #####
  ########## append to input.csv the results - #years with avgFP > threshold and #years with propdaySFP > 0.5 
  #####
  # I should probably leave off some of the first few year - to let to model equilibrate 
  
  # prop years with avgFP > regimethreshold
  propyears_avgFP_abovethreshold <- sum(data_nutrients$avgFP >= regimethreshold)/years
  
  # prop years with propdaysFP > 0.5
  propyears_propdaysFP_abovehalf <- sum(data_nutrients$propdaysFP >= 0.5)/years
  
  assign("propyears_avgFP_abovethreshold", propyears_avgFP_abovethreshold, envir=.GlobalEnv)

  assign("propyears_propdaysFP_abovehalf", propyears_propdaysFP_abovehalf, envir=.GlobalEnv)
  
  ########################################################################
  ########### write a .txt of ALL parameter values in workspace ##########
  ########################################################################
  sink(file=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," parameters", ".txt", sep=""))
  print(ls.str(envir = .GlobalEnv),max.level=0)
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