STEP10 <- function() { # start defining the function 
  
  #for (i in 1:((timesteps+1)*years)) { # loop through time steps <--- change back to the original when doing real model
  for (i in 1:9) { # loop through time steps  
  
    # check if it's NOT overwintering timestep, then do the following 
    if (i %ni% winters) { 
    
      ########
      # MOVE #
      ########
      if (numbspecies == 4) { 
        LIST[[i]]$SP4matrix <- MOVE(LIST[[i]]$SP4matrix) 
        LIST[[i]]$SP3matrix <- MOVE(LIST[[i]]$SP3matrix)
        LIST[[i]]$SP2matrix <- MOVE(LIST[[i]]$SP2matrix)
        LIST[[i]]$SP1matrix <- MOVE(LIST[[i]]$SP1matrix)
        LIST[[i]]$SPALLmatrix <- LIST[[i]]$SP4matrix + LIST[[i]]$SP3matrix + LIST[[i]]$SP2matrix + LIST[[i]]$SP1matrix
      }
      else if (numbspecies == 3) {
        LIST[[i]]$SP3matrix <- MOVE(LIST[[i]]$SP3matrix)
        LIST[[i]]$SP2matrix <- MOVE(LIST[[i]]$SP2matrix)
        LIST[[i]]$SP1matrix <- MOVE(LIST[[i]]$SP1matrix) 
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP3matrix + LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix
      }
      else if (numbspecies == 2) {
        LIST[[i]]$SP2matrix <- MOVE(LIST[[i]]$SP2matrix)
        LIST[[i]]$SP1matrix <- MOVE(LIST[[i]]$SP1matrix) 
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix + LIST[[i]]$SPALLmatrix
      }
      else if (numbspecies == 1) {
        LIST[[i]]$SP1matrix <- MOVE(LIST[[i]]$SP1matrix) 
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP1matrix
      }
      
      ########
      # GROW #
      ########
      if (numbspecies == 4) { 
        LIST[[i+1]]$SP4matrix <- GROW(LIST[[i]]$SP4matrix, LIST[[i+1]]$SP4matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SP3matrix <- GROW(LIST[[i]]$SP3matrix, LIST[[i+1]]$SP3matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SP2matrix <- GROW(LIST[[i]]$SP2matrix, LIST[[i+1]]$SP2matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SP1matrix <- GROW(LIST[[i]]$SP1matrix, LIST[[i+1]]$SP1matrix, LIST[[i]]$SPALLmatrix)
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP4matrix + LIST[[i+1]]$SP3matrix + LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix
      }
      else if (numbspecies == 3) {
        LIST[[i+1]]$SP3matrix <- GROW(LIST[[i]]$SP3matrix, LIST[[i+1]]$SP3matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SP2matrix <- GROW(LIST[[i]]$SP2matrix, LIST[[i+1]]$SP2matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SP1matrix <- GROW(LIST[[i]]$SP1matrix, LIST[[i+1]]$SP1matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP3matrix + LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix
      }
      else if (numbspecies == 2) {
        LIST[[i+1]]$SP2matrix <- GROW(LIST[[i]]$SP2matrix, LIST[[i+1]]$SP2matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SP1matrix <- GROW(LIST[[i]]$SP1matrix, LIST[[i+1]]$SP1matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP2matrix + LIST[[i+1]]$SP1matrix + LIST[[i]]$SPALLmatrix
      }
      else if (numbspecies == 1) {
        LIST[[i+1]]$SP1matrix <- GROW(LIST[[i]]$SP1matrix, LIST[[i+1]]$SP1matrix, LIST[[i]]$SPALLmatrix) 
        LIST[[i+1]]$SPALLmatrix <- LIST[[i+1]]$SP1matrix
      }

      ############
      # UPTAKE N #
      ############
      LIST[[i+1]]$TOTALN <- UPTAKE_N(LIST,i)
      
      ############
      # UPTAKE P #
      ############
      LIST[[i+1]]$TOTALP <- UPTAKE_P(LIST,i)
      
    } # closes the if statement, when it is not an overwintering step 
    
    # otherwise, it IS an overwintering timestep, do the following
    else {   
      
    } # closes the else statement - when it is an overwintering step
    
    ########
    # PLOT #
    ########
    require(raster)
    if (numbspecies == 4) { 
      par(mfrow=c(numbspecies+1,1))
      plot(raster(LIST[[i]]$SP1matrix),main=paste("Species 1",i,sep=" "))
      plot(raster(LIST[[i]]$SP2matrix),main=paste("Species 2",i,sep=" "))
      plot(raster(LIST[[i]]$SP3matrix),main=paste("Species 3",i,sep=" "))
      plot(raster(LIST[[i]]$SP4matrix),main=paste("Species 4",i,sep=" "))
      plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species",i,sep=" "))
    }
    else if (numbspecies == 3) { 
      par(mfrow=c(numbspecies+1,1))
      plot(raster(LIST[[i]]$SP1matrix),main=paste("Species 1",i,sep=" "))
      plot(raster(LIST[[i]]$SP2matrix),main=paste("Species 2",i,sep=" "))
      plot(raster(LIST[[i]]$SP3matrix),main=paste("Species 3",i,sep=" "))
      plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species",i,sep=" "))
    }
    else if (numbspecies == 2) { 
      par(mfrow=c(numbspecies+1,1))
      plot(raster(LIST[[i]]$SP1matrix),main=paste("Species 1",i,sep=" "))
      plot(raster(LIST[[i]]$SP2matrix),main=paste("Species 2",i,sep=" "))
      plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species",i,sep=" "))
    }
    else if (numbspecies == 2) { 
      par(mfrow=c(numbspecies+1,1))
      plot(raster(LIST[[i]]$SP1matrix),main=paste("Species 1",i,sep=" "))
      plot(raster(LIST[[i]]$SPALLmatrix),main=paste("All species",i,sep=" "))
    }
  } # closes for loop through time steps 
  
  return(LIST)
}

# define a new operator - opposite of %in% - i.e., not in a subset
"%ni%" <- Negate("%in%") 