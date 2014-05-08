STEP12 <- function() { # start defining the function 
  
  for (i in 1:((timesteps+1)*years)) { # loop through time steps 
  
    # check if it's NOT overwintering timestep, then do the following 
    if (i %ni% winters) { 
    
      ########
      # MOVE #
      ########
      if (numbFPspecies == 4) { 
        LIST[[i]]$SAVmatrix <- MOVE_SAV(LIST[[i]]$SAVmatrix) 
        LIST[[i]]$FP4matrix <- MOVE_FP(LIST[[i]]$FP4matrix) 
        LIST[[i]]$FP3matrix <- MOVE_FP(LIST[[i]]$FP3matrix)
        LIST[[i]]$FP2matrix <- MOVE_FP(LIST[[i]]$FP2matrix)
        LIST[[i]]$FP1matrix <- MOVE_FP(LIST[[i]]$FP1matrix)
        LIST[[i]]$FPALLmatrix <- LIST[[i]]$FP4matrix + LIST[[i]]$FP3matrix + LIST[[i]]$FP2matrix + LIST[[i]]$FP1matrix
      }
      else if (numbFPspecies == 3) {
        LIST[[i]]$SAVmatrix <- MOVE_SAV(LIST[[i]]$SAVmatrix)
        LIST[[i]]$FP3matrix <- MOVE_FP(LIST[[i]]$FP3matrix)
        LIST[[i]]$FP2matrix <- MOVE_FP(LIST[[i]]$FP2matrix)
        LIST[[i]]$FP1matrix <- MOVE_FP(LIST[[i]]$FP1matrix)
        LIST[[i]]$FPALLmatrix <- LIST[[i]]$FP3matrix + LIST[[i]]$FP2matrix + LIST[[i]]$FP1matrix
      }
      else if (numbFPspecies == 2) {
        LIST[[i]]$SAVmatrix <- MOVE_SAV(LIST[[i]]$SAVmatrix)
        LIST[[i]]$FP2matrix <- MOVE_FP(LIST[[i]]$FP2matrix)
        LIST[[i]]$FP1matrix <- MOVE_FP(LIST[[i]]$FP1matrix)
        LIST[[i]]$FPALLmatrix <- LIST[[i]]$FP2matrix + LIST[[i]]$FP1matrix + LIST[[i]]$FPALLmatrix
      }
      else if (numbFPspecies == 1) {
        LIST[[i]]$SAVmatrix <- MOVE_SAV(LIST[[i]]$SAVmatrix)
        LIST[[i]]$FP1matrix <- MOVE_FP(LIST[[i]]$FP1matrix)
        LIST[[i]]$FPALLmatrix <- LIST[[i]]$FP1matrix
      }
      
      ########
      # WIND #
      ########
      if (numbFPspecies == 4) { 
        LIST[[i]]$FP4matrix <- WIND(LIST[[i]]$FP4matrix) 
        LIST[[i]]$FP3matrix <- WIND(LIST[[i]]$FP3matrix)
        LIST[[i]]$FP2matrix <- WIND(LIST[[i]]$FP2matrix)
        LIST[[i]]$FP1matrix <- WIND(LIST[[i]]$FP1matrix)
        LIST[[i]]$FPALLmatrix <- LIST[[i]]$FP4matrix + LIST[[i]]$FP3matrix + LIST[[i]]$FP2matrix + LIST[[i]]$FP1matrix
      }
      else if (numbFPspecies == 3) {
        LIST[[i]]$FP3matrix <- WIND(LIST[[i]]$FP3matrix)
        LIST[[i]]$FP2matrix <- WIND(LIST[[i]]$FP2matrix)
        LIST[[i]]$FP1matrix <- WIND(LIST[[i]]$FP1matrix)
        LIST[[i]]$FPALLmatrix <- LIST[[i]]$FP3matrix + LIST[[i]]$FP2matrix + LIST[[i]]$FP1matrix
      }
      else if (numbFPspecies == 2) {
        LIST[[i]]$FP2matrix <- WIND(LIST[[i]]$FP2matrix)
        LIST[[i]]$FP1matrix <- WIND(LIST[[i]]$FP1matrix)
        LIST[[i]]$FPALLmatrix <- LIST[[i]]$FP2matrix + LIST[[i]]$FP1matrix + LIST[[i]]$FPALLmatrix
      }
      else if (numbFPspecies == 1) {
        LIST[[i]]$FP1matrix <- WIND(LIST[[i]]$FP1matrix)
        LIST[[i]]$FPALLmatrix <- LIST[[i]]$FP1matrix
      }
            
      ########
      # GROW #
      ########
      # FP growth
      if (numbFPspecies == 4) { 
        LIST[[i+1]]$SAVmatrix <- GROW_SAV(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, LIST[[i]]$FPALLmatrix, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP4matrix <- GROW_FP(LIST[[i]]$FP4matrix, LIST[[i+1]]$FP4matrix, LIST[[i]]$FPALLmatrix, n=4, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP3matrix <- GROW_FP(LIST[[i]]$FP3matrix, LIST[[i+1]]$FP3matrix, LIST[[i]]$FPALLmatrix, n=3, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP2matrix <- GROW_FP(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, LIST[[i]]$FPALLmatrix, n=2, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP1matrix <- GROW_FP(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, LIST[[i]]$FPALLmatrix, n=1, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN)
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP4matrix + LIST[[i+1]]$FP3matrix + LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix
      }
      else if (numbFPspecies == 3) {
        LIST[[i+1]]$SAVmatrix <- GROW_SAV(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, LIST[[i]]$FPALLmatrix, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP3matrix <- GROW_FP(LIST[[i]]$FP3matrix, LIST[[i+1]]$FP3matrix, LIST[[i]]$FPALLmatrix, n=3, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP2matrix <- GROW_FP(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, LIST[[i]]$FPALLmatrix, n=2, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP1matrix <- GROW_FP(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, LIST[[i]]$FPALLmatrix, n=1, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP3matrix + LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix
      }
      else if (numbFPspecies == 2) {
        LIST[[i+1]]$SAVmatrix <- GROW_SAV(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, LIST[[i]]$FPALLmatrix, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN)
        LIST[[i+1]]$FP2matrix <- GROW_FP(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, LIST[[i]]$FPALLmatrix, n=2, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FP1matrix <- GROW_FP(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, LIST[[i]]$FPALLmatrix, n=1, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix + LIST[[i]]$FPALLmatrix
      }
      else if (numbFPspecies == 1) {
        LIST[[i+1]]$SAVmatrix <- GROW_SAV(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, LIST[[i]]$FPALLmatrix, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN)
        LIST[[i+1]]$FP1matrix <- GROW_FP(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, LIST[[i]]$FPALLmatrix, n=1, LIST[[i]]$TOTALP, LIST[[i]]$TOTALN) 
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP1matrix
      }

      ############
      # UPTAKE N #
      ############
      LIST[[i+1]]$TOTALN <- UPTAKE_N2(LIST,i,n=numbFPspecies)
      
      ############
      # UPTAKE P #
      ############
      LIST[[i+1]]$TOTALP <- UPTAKE_P2(LIST,i,n=numbFPspecies)
      
    } # closes the if statement, when it is not an overwintering step 
    
    # otherwise, it IS an overwintering timestep, do the following
    ##############
    # OVERWINTER #
    ##############
    else {   
      if (numbFPspecies == 4) { 
        LIST[[i+1]]$FP4matrix <- OVERWINTER(LIST[[i]]$FP4matrix, LIST[[i+1]]$FP4matrix, n=5)
        LIST[[i+1]]$FP3matrix <- OVERWINTER(LIST[[i]]$FP3matrix, LIST[[i+1]]$FP3matrix, n=4)
        LIST[[i+1]]$FP2matrix <- OVERWINTER(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, n=3)
        LIST[[i+1]]$FP1matrix <- OVERWINTER(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, n=2)
        LIST[[i+1]]$SAVmatrix <- OVERWINTER(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, n=1)
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP4matrix + LIST[[i+1]]$FP3matrix + LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix
      }
      else if (numbFPspecies == 3) {
        LIST[[i+1]]$FP3matrix <- OVERWINTER(LIST[[i]]$FP3matrix, LIST[[i+1]]$FP3matrix, n=4)
        LIST[[i+1]]$FP2matrix <- OVERWINTER(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, n=3)
        LIST[[i+1]]$FP1matrix <- OVERWINTER(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, n=2)
        LIST[[i+1]]$SAVmatrix <- OVERWINTER(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, n=1)
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP3matrix + LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix
      }
      else if (numbFPspecies == 2) {
        LIST[[i+1]]$FP2matrix <- OVERWINTER(LIST[[i]]$FP2matrix, LIST[[i+1]]$FP2matrix, n=3)
        LIST[[i+1]]$FP1matrix <- OVERWINTER(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, n=2)
        LIST[[i+1]]$SAVmatrix <- OVERWINTER(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, n=1)
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP2matrix + LIST[[i+1]]$FP1matrix
      }
      else if (numbFPspecies == 1) {
        LIST[[i+1]]$FP1matrix <- OVERWINTER(LIST[[i]]$FP1matrix, LIST[[i+1]]$FP1matrix, n=2)
        LIST[[i+1]]$SAVmatrix <- OVERWINTER(LIST[[i]]$SAVmatrix, LIST[[i+1]]$SAVmatrix, n=1)
        LIST[[i+1]]$FPALLmatrix <- LIST[[i+1]]$FP1matrix
      }
      
      #############
      # RELEASE N #
      #############
      LIST[[i+1]]$TOTALN <- RELEASE_N2(LIST,i)
      
      #############
      # RELEASE P #
      #############
      LIST[[i+1]]$TOTALP <- RELEASE_P2(LIST,i)
      
    } # closes the else statement - when it is an overwintering step
    
    ########
    # PLOT #
    ########
    require(raster)
    
    #new plotting: zlim should set the constant scale 
    par(mfrow=c(2,1))
    plot(raster(LIST[[i]]$SAVmatrix),main=paste("SAV","Timestep:",i,sep=" "), zlim=c(0,100))
    plot(raster(LIST[[i]]$FPALLmatrix),main=paste("All FP species"), zlim=c(0,100))

    
  } # closes for loop through time steps 
  
  print(LIST[[i]])
  
  return(LIST)
}

# define a new operator - opposite of %in% - i.e., not in a subset
"%ni%" <- Negate("%in%") 