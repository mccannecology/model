STEP11 <- function() { # start defining the function 
  
  for (i in 1:((timesteps+1)*years)) { # loop through time steps 
  
    # check if it's NOT overwintering timestep, then do the following 
    if (i %ni% winters) { 
    
      ########
      # MOVE #
      ########
      if (numbFPspecies == 4) { 
        LIST[[i]]$SP4matrix <- MOVE2(LIST[[i]]$SP4matrix) 
        LIST[[i]]$SP3matrix <- MOVE2(LIST[[i]]$SP3matrix)
        LIST[[i]]$SP2matrix <- MOVE2(LIST[[i]]$SP2matrix)
        LIST[[i]]$SP1matrix <- MOVE2(LIST[[i]]$SP1matrix)
        LIST[[i]]$SPALLmatrix <- LIST[[i]]$SP4matrix + LIST[[i]]$SP3matrix + LIST[[i]]$SP2matrix + LIST[[i]]$SP1matrix
      }
      else if (numbFPspecies == 3) {
        LIST[[i]]$SP3matrix <- MOVE2(LIST[[i]]$SP3matrix)
        LIST[[i]]$SP2matrix <- MOVE2(LIST[[i]]$SP2matrix)
        LIST[[i]]$SP1matrix <- MOVE2(LIST[[i]]$SP1matrix)
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP3matrix + LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix
      }
      else if (numbFPspecies == 2) {
        LIST[[i]]$SP2matrix <- MOVE2(LIST[[i]]$SP2matrix)
        LIST[[i]]$SP1matrix <- MOVE2(LIST[[i]]$SP1matrix)
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix + LIST[[i]]$SPALLmatrix
      }
      else if (numbFPspecies == 1) {
        LIST[[i]]$SP1matrix <- MOVE2(LIST[[i]]$SP1matrix)
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP1matrix
      }
      
      ########
      # GROW #
      ########
      # SAV growth 
      LIST[[i+1]]$SAVmatrix <- GROW_SAV(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, LIST[[i]]$FPALLmatrix, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
      
      # FP growth
      if (numbFPspecies == 4) { 
        LIST[[i+1]]$FP4matrix <- GROW_FP(LIST[[i]]$FP4matrix, LIST[[i+1]]$FP4matrix, LIST[[i]]$FPALLmatrix, n=4, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP3matrix <- GROW_FP(LIST[[i]]$FP3matrix, LIST[[i+1]]$FP3matrix, LIST[[i]]$FPALLmatrix, n=3, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP2matrix <- GROW_FP(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, LIST[[i]]$FPALLmatrix, n=2, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP1matrix <- GROW_FP(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, LIST[[i]]$FPALLmatrix, n=1, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN)
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP4matrix + LIST[[i+1]]$FP3matrix + LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix
      }
      else if (numbFPspecies == 3) {
        LIST[[i+1]]$FP3matrix <- GROW_FP(LIST[[i]]$FP3matrix, LIST[[i+1]]$FP3matrix, LIST[[i]]$FPALLmatrix, n=3, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP2matrix <- GROW_FP(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, LIST[[i]]$FPALLmatrix, n=2, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP1matrix <- GROW_FP(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, LIST[[i]]$FPALLmatrix, n=1, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP3matrix + LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix
      }
      else if (numbFPspecies == 2) {
        LIST[[i+1]]$FP2matrix <- GROW_FP(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, LIST[[i]]$FPALLmatrix, n=2, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP1matrix <- GROW_FP(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, LIST[[i]]$FPALLmatrix, n=1, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix + LIST[[i]]$FPALLmatrix
      }
      else if (numbFPspecies == 1) {
        LIST[[i+1]]$FP1matrix <- GROW_FP(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, LIST[[i]]$FPALLmatrix, n=1, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP1matrix
      }

      ############
      # UPTAKE N #
      ############
      LIST[[i+1]]$TOTALN <- UPTAKE_N(LIST,i,n=numbFPspecies)
      
      ############
      # UPTAKE P #
      ############
      LIST[[i+1]]$TOTALP <- UPTAKE_P(LIST,i,n=numbFPspecies)
      
    } # closes the if statement, when it is not an overwintering step 
    
    # otherwise, it IS an overwintering timestep, do the following
    ##############
    # OVERWINTER #
    ##############
    else {   
      if (numbFPspecies == 4) { 
        LIST[[i+1]]$SP4matrix <- OVERWINTER(LIST[[i]]$SP4matrix, LIST[[i+1]]$SP4matrix, n=4)
        LIST[[i+1]]$SP3matrix <- OVERWINTER(LIST[[i]]$SP3matrix, LIST[[i+1]]$SP3matrix, n=3)
        LIST[[i+1]]$SP2matrix <- OVERWINTER(LIST[[i]]$SP2matrix, LIST[[i+1]]$SP2matrix, n=2)
        LIST[[i+1]]$SP1matrix <- OVERWINTER(LIST[[i]]$SP1matrix, LIST[[i+1]]$SP1matrix, n=1)
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP4matrix + LIST[[i+1]]$SP3matrix + LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix
      }
      else if (numbFPspecies == 3) {
        LIST[[i+1]]$SP3matrix <- OVERWINTER(LIST[[i]]$SP3matrix, LIST[[i+1]]$SP3matrix, n=3)
        LIST[[i+1]]$SP2matrix <- OVERWINTER(LIST[[i]]$SP2matrix, LIST[[i+1]]$SP2matrix, n=2)
        LIST[[i+1]]$SP1matrix <- OVERWINTER(LIST[[i]]$SP1matrix, LIST[[i+1]]$SP1matrix, n=1)
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP3matrix + LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix
      }
      else if (numbFPspecies == 2) {
        LIST[[i+1]]$SP2matrix <- OVERWINTER(LIST[[i]]$SP2matrix, LIST[[i+1]]$SP2matrix, n=2)
        LIST[[i+1]]$SP1matrix <- OVERWINTER(LIST[[i]]$SP1matrix, LIST[[i+1]]$SP1matrix, n=1)
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix
      }
      else if (numbFPspecies == 1) {
        LIST[[i+1]]$SP1matrix <- OVERWINTER(LIST[[i]]$SP1matrix, LIST[[i+1]]$SP1matrix, n=1)
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP1matrix
      }
      
      #############
      # RELEASE N #
      #############
      LIST[[i+1]]$TOTALN <- RELEASE_N(LIST,i)
      
      #############
      # RELEASE P #
      #############
      LIST[[i+1]]$TOTALP <- RELEASE_P(LIST,i)
      
    } # closes the else statement - when it is an overwintering step
    
    ########
    # PLOT #
    ########
    require(raster)
    
    # new plotting: zlim should set the constant scale 
    plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species","Timestep:",i,sep=" "), zlim=c(0,100))
    
    #if (numbFPspecies == 4) { 
    #  par(mfrow=c(numbFPspecies+1,1))
    #  plot(raster(LIST[[i]]$SP1matrix),main=paste("Species 1","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SP2matrix),main=paste("Species 2","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SP3matrix),main=paste("Species 3","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SP4matrix),main=paste("Species 4","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species","Timestep:",i,sep=" "))
    #}
    #else if (numbFPspecies == 3) { 
    #  par(mfrow=c(numbFPspecies+1,1))
    #  plot(raster(LIST[[i]]$SP1matrix),main=paste("Species 1","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SP2matrix),main=paste("Species 2","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SP3matrix),main=paste("Species 3","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species","Timestep:",i,sep=" "))
    #}
    #else if (numbFPspecies == 2) { 
    #  par(mfrow=c(numbFPspecies+1,1))
    #  plot(raster(LIST[[i]]$SP1matrix),main=paste("Species 1","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SP2matrix),main=paste("Species 2","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species","Timestep:",i,sep=" "))
    #}
    #else if (numbFPspecies == 2) { 
    #  par(mfrow=c(numbFPspecies+1,1))
    #  plot(raster(LIST[[i]]$SP1matrix),main=paste("Species 1","Timestep:",i,sep=" "))
    #  plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species","Timestep:",i,sep=" "))
    #}
  } # closes for loop through time steps 
  
  return(LIST)
}

# define a new operator - opposite of %in% - i.e., not in a subset
"%ni%" <- Negate("%in%") 