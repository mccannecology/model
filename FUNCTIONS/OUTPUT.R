#########################################################
# Outputs model results                                 #
# Replaces functions ANIMATE() and PLOT()               #
#                                                       #
# By: Michael J. McCann                                 #
# Last Updated: 2/26/2014                               #
######################################################### 

OUTPUT <- function(animate=FALSE){  
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
        plot(raster(LIST[[i]]$PAmatrix),main=i)
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
  
  for(i in 1:(1+(timesteps+1)*years)) image[,,i] = LIST[[i]]$PAmatrix
  
  write.gif(image, filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," animate", ".gif", sep=""), 
            scale="always", col="jet.colors")  
  
  ########################################################################################  
  ############### Plot population of all species through time - GGPLOT2 ##################
  ########################################################################################
  require(ggplot2)
  require(reshape2)
  
  # creates a blank data frame where this all will go 
  data01<-NULL
  data01<-as.data.frame(data01)
  
  # for each timestep, for each species, assign the popl. size to the appropriate position in data01
  for (i in 1:(1+(timesteps+1)*years)) { 
    for (n in 1:numbspecies){
      data01[i,n]<-sum(LIST[[i]]$PAmatrix == n)
    }
  }
  
  # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
  names<-NULL
  for (n in 1:numbspecies){
    names<-append(names, paste("species",n,sep="",collapse=""))
  }
  
  # assign that vector to the column names of your dataframe 
  names(data01)<-names
  
  # add time to your dataframe 
  data01$time <- seq(1,1+(timesteps+1)*years,1)
  
  #reshape your data 1st before trying ggplot2 
  data01melt <- melt(data01,id.vars="time")
  
  ggplot(data01melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("population size")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," popl size", ".jpg", sep=""))
  
  dev.off()
  
  #################################################################
  ############# Plot % cover through time - GGPLOT2 ###############
  #################################################################
  coverALL <- seq(from=0,to=0,length=(1+(timesteps+1)*years))
  cover01 <- seq(from=0,to=0,length=(1+(timesteps+1)*years))
  cover02 <- seq(from=0,to=0,length=(1+(timesteps+1)*years))
  time <- seq(1,1+(timesteps+1)*years,1)
  
  for (i in 1:(1+(timesteps+1)*years)) {
    coverALL[i] <- (sum(LIST[[i]]$PAmatrix >= 1) / (width*height) * 100)
    cover01[i] <- (sum(LIST[[i]]$PAmatrix == 1) / (width*height) * 100)
    cover02[i] <- (sum(LIST[[i]]$PAmatrix == 2) / (width*height) * 100)
  }        
  
  # creates a blank data frame where this all will go 
  data02<-NULL
  data02<-as.data.frame(data02)
  
  # for each timestep, for each species, assign the percent cover to the appropriate position in data01
  for (i in 1:(1+(timesteps+1)*years)) { 
    for (n in 1:numbspecies){
      data02[i,n] <- (sum(LIST[[i]]$PAmatrix == n) / (width*height) * 100)
    }
  }
  
  # generate a vector of ("species1","species2",etc.) - to use for naming your columns 
  names<-NULL
  for (n in 1:numbspecies){
    names<-append(names, paste("cover_sp",n,sep="",collapse=""))
  }
  
  # assign that vector to the column names of your dataframe 
  names(data02)<-names
  
  ## Make the last column COVERALL 
  for (i in 1:(1+(timesteps+1)*years)) {
    data02$cover_ALL[i] <- (sum(LIST[[i]]$PAmatrix >= 1) / (width*height) * 100)
  }
  
  # add time to your dataframe 
  data02$time <- seq(1,1+(timesteps+1)*years,1)
  
  # reshape your data 1st before trying ggplot2 
  data02melt <- melt(data02,id.vars="time")
  
  ggplot(data02melt, aes(x=time,y=value,colour=variable)) + geom_line() + ylab("percent cover")
  ggsave(filename=paste(format(Sys.time(), "%m-%d-%Y-%H%M")," percent cover", ".jpg", sep=""))
  
  dev.off()
  #################################################################
  ############# Count # years waterbody is in FP regime ###########
  #################################################################
  # make a vector of "year"
  year <- NULL
  for (i in 1:years){
    year <- append(year,rep(i,timesteps+1))
  }
  year <- append(year, i+1) # this is for the first day of the next year 

  # add year to your dataframe 
  data02$year <- year
  
  
  # Average floating plant coverage for all time steps in each year 
  avgFP <- aggregate(data02$cover_ALL,list(year=data02$year),mean) 
  colnames(avgFP)[2] <- "avgFP"
  
  # Number of days each year that a water is above a treshold value of cover_ALL
  regimethreshold <- 70 
  apply.fun <- function(x) {
    sum(x > regimethreshold)
  }
  daysFP <- aggregate(data02$cover_ALL,list(year=data02$year),apply.fun) 
  colnames(daysFP)[2] <- "daysFP"
  
  # Proportion of days each year that a waterbody is above a treshold value of cover_ALL
  regimethreshold <- 70 
  apply.fun <- function(x) {
    sum(x > regimethreshold)/timesteps
  }
  propdaysFP <- aggregate(data02$cover_ALL,list(year=data02$year),apply.fun) 
  colnames(propdaysFP)[2] <- "propdaysFP"
  
  ####
  ####### trying to get the first day that the waterbody is above a threshold value of cover_ALL 
  ####### using aggregrate()
  ####
  regimethreshold <- 70 
  apply.fun <- function(x) {
    min(x > regimethreshold)
  }
  aggregate(data02$cover_ALL,list(year=data02$year),apply.fun) 
    
  ####
  ####### trying to get the first day that the waterbody is above a threshold value of cover_ALL 
  ####### try it again, this time by looping   
  ####
  for (i in 1:timesteps+1) { # loop through time steps
    for (j in 1:years) { # loop by years 
      min(data02$cover_ALL[i*j] > 70)
      # this logical test works, but I still need to somehow return the index of the cover_ALL values that exceeds the threshold 
    }
  }
  
  # Build a data frame with all of these different summary statistics for each year 
  data03 <- merge(avgFP,daysFP)
  data03 <- merge(data03, propdaysFP)
  # assign it to something useful otuside of the function 
  # I can probably do this smarter than just repeated merge()
  
  ########
  ############# assign these values to the output file
  ########
  ############# Need to fix this so the value actually gets assigned to the parameters data frame outside of the function 
  ########
  regimethreshold <- 70 
  #parameters$propcyearsFP[simulnumb] <- nrow(subset(avgFPcover[2], avgFPcover[2] > regimethreshold))/years
  assign("parameters$propcyearsFP[simulnumb]", (nrow(subset(avgFPcover[2], avgFPcover[2] > regimethreshold))/years), envir=.GlobalEnv)
  
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