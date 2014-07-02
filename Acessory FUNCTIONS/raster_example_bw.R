require(raster)
a<-matrix(100,100,data=rpois(10000,150))

r<-raster(a)

plot(r)

threshold<-170

occupied<-r>threshold

#pres<-Which(r > threshold,cells=TRUE)

birth=function(x){
  out<-rpois(1,mean(x))
  return(out)
}

growth <- focal(occupied,matrix(1,3,3),birth,pad=TRUE,padValue=0)

names(growth)<-"growth"
names(occupied)<-"occupied"

new_biomass<-occupied + growth
names(new_biomass)<-"new_biomass"
plot(stack(occupied,growth,new_biomass))

g<-new_biomass
plot(mean(stack(g,new_biomass)))
