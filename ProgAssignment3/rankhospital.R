rankhospital <- function(state, outcome, num = "best") {
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
 
  
  outcomedat[, 11] <- as.numeric(outcomedat[, 11])  #HA
  outcomedat[, 17] <- as.numeric(outcomedat[, 17])  #HF
  outcomedat[, 23] <- as.numeric(outcomedat[, 23])  #PN
  stdat<-outcomedat[which(outcomedat$State == state),c(2,11,17,23)]
 
  if(!state%in%states || !outcome%in%outcomes){
    
    stop("Enter valid state and outcome")
    
  }
  
  ## Return hospital name in that state with the given rank
  
  
  if(outcome == outcomes[1]){
    stdat<-stdat[,c(1,2)]
    order.ranks<-order(stdat[2],stdat[1])
    stdat$rank<-NA
    stdat$rank[order.ranks] <- 1:nrow(stdat)
    stdat<-stdat[order.ranks,]
    stdat<-na.omit(stdat,2)
    count<-nrow(stdat)
    
    
    #View(stdat)
    if(num=="best"){
      stdat[1,1]
    }
    else if(num == "worst"){
      stdat[nrow(stdat),1]
    }
    else{
      if(num>count){
        print(count)
        return("NA")
      }
      stdat[num,1]
    }
    
  }
  else if(outcome == outcomes[2]){
    stdat<-stdat[,c(1,3)]
    order.ranks<-order(stdat[2],stdat[1])
    stdat$rank<-NA
    stdat$rank[order.ranks] <- 1:nrow(stdat)
    stdat<-stdat[order.ranks,]
    stdat<-na.omit(stdat,2)
    count<-nrow(stdat)
    
    #View(stdat)
    
    if(num=="best"){
      stdat[1,1]
    }
    else if(num == "worst"){
      stdat[nrow(stdat),1]
    }
    else{
      if(num>count){
        print(count)
        return("NA")
      }
      stdat[num,1]
    }

  }
  else{
    stdat<-stdat[,c(1,4)]
    order.ranks<-order(stdat[2],stdat[1])
    stdat$rank<-NA
    stdat$rank[order.ranks] <- 1:nrow(stdat)
    stdat<-stdat[order.ranks,]
    stdat<-na.omit(stdat,2)
    count<-nrow(stdat)
    
    #View(stdat)
    if(num=="best"){
      stdat[1,1]
    }
    else if(num == "worst"){
      stdat[nrow(stdat),1]
    }
    else{
      if(num>count){
        print(count)
        return("NA")
      }
      stdat[num,1]
    }
    

  }
  
  ## 30-day death rate
}
