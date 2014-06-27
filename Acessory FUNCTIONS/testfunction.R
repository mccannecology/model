testfunction<-function() {
  today<-LIST[[1]]
  for (t in 1:timesteps){
    tomorrow<-STEP20(today)
    LIST[[t+1]]<-tomorrow
    today<-tomorrow
  }  
}
