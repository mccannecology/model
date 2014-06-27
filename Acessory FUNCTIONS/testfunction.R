testfunction <- function() {
  # for loop - STEP20() to the entire LIST
  today<-LIST[[1]]
  for (t in 1:timesteps){
    tomorrow<-STEP20(today,t)
    LIST[[t+1]]<-tomorrow
    today<-tomorrow
  }    
  return(LIST)
}