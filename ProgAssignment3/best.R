best <- function(state, outcome) {
  ## Read outcome data
  outcomedat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  
  
  states<-unique(outcomedat$State)
  outcomes<-c("heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  if(!state%in%states){
    print("INVALID STATE")
  }
  if(!outcome%in%outcomes){
    print("INVALID OUTCOME")
  }
  if(!state%in%states || !outcome%in%outcomes){
    
    stop("Enter valid state and outcomer")
    
  }
  outcomedat[, 11] <- as.numeric(outcomedat[, 11])  #HA
  outcomedat[, 17] <- as.numeric(outcomedat[, 17])  #HF
  outcomedat[, 23] <- as.numeric(outcomedat[, 23])  #PN
  
  ## Return hospital name in that state with lowest 30-day death
  stdat<-outcomedat[which(outcomedat$State == state),c(2,11,17,23)]
  ha<-min(stdat[2],na.rm = TRUE)
  hf<-min(stdat[3],na.rm = TRUE)
  pn<-min(stdat[4],na.rm = TRUE)
  
  if(outcome == outcomes[1]){
    result<-stdat[which(stdat[,2] == ha),]
    result<-result[order(result[1])]
  }
  else if(outcome == outcomes[2]){
    result<-stdat[which(stdat[,3] == hf),]
    result<-result[order(result[1])]
  }
  else{
    result<-stdat[which(stdat[,4] == pn),]
    result<-result[order(result[1])]
  }
  
  print(result[1,1])
  ## rate
  
}
