# Ideas from: 
# http://ryouready.wordpress.com/2010/11/21/animate-gif-images-in-r-imagemagick/

require(raster)

################### 
# Floating plants #
# Submerged plants#
###################
dir.create("FP")
setwd("FP")

png(file="FP%03d.png", width=375, height=375)
for(i in 1:(timesteps+1)) { # loop through time steps
  plot(raster(LIST[[i]]$FPtotal),main=paste("Floating Plants ","Timestep ",i,sep=""),zlim=c(0,100))
}
dev.off()

setwd("C:/All Data/Mike M/model/") # return to the original wd 

#################### 
# Submerged plants #
####################
dir.create("SAV")
setwd("SAV")

# example 1: simple animated countdown from 10 to "GO!".
png(file="SAV%03d.png", width=375, height=375)
for(i in 1:(timesteps+1)) { # loop through time steps
  plot(raster(LIST[[i]]$SAV),main="Submerged Plants ",zlim=c(0,100))
}
dev.off()

########### 
# Convert #
###########
# use command line (e.g., bash)
# navigate to each working directory 
# type "convert *.png example_1.gif"

# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))
