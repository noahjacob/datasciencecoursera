rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomedat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")  
  
  
  states<-unique(outcomedat$State)
  outcomes<-c("heart attack","heart failure","pneumonia")
  
  ## Check that state and outcome are valid
  
  if(!outcome%in%outcomes){
    print("INVALID OUTCOME")
  }
  
  
  outcomedat[, 11] <- as.numeric(outcomedat[, 11])  #HA
  outcomedat[, 17] <- as.numeric(outcomedat[, 17])  #HF
  outcomedat[, 23] <- as.numeric(outcomedat[, 23])  #PN
  
  if(!outcome%in%outcomes){
    
    stop("Enter valid state and outcome")
    
  }
  Hospital<-c()
  State<-c()
  ## For each state, find the hospital of the given rank
  for(state in states){
    stdat<-outcomedat[which(outcomedat$State == state),c(2,11,17,23)]
    if(outcome == outcomes[1]){
      stdat<-stdat[,c(1,2)]
      order.ranks<-order(stdat[2],stdat[1])
      stdat$rank<-NA
      stdat$rank[order.ranks] <- 1:nrow(stdat)
      stdat<-stdat[order.ranks,]
      stdat<-na.omit(stdat,2)
      count<-nrow(stdat)
      if(num>count){
        print(count)
        Hospital<-c(Hospital,"NA")
        
        next
      }
      
      #View(stdat)
      if(num=="best"){
        Hospital<-c(Hospital,stdat[1,1])
        
      }
      else if(num == "worst"){
        Hospital<-c(Hospital,stdat[nrow(stdat),1])
      }
      else{
        
        Hospital<-c(Hospital,stdat[num,1])
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
        Hospital<-c(Hospital,stdat[1,1])
        
      }
      else if(num == "worst"){
        Hospital<-c(Hospital,stdat[nrow(stdat),1])
      }
      else{
        if(num>count){
          print(count)
          Hospital<-c(Hospital,"NA")
          
          next
        }
        
        Hospital<-c(Hospital,stdat[num,1])
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
        Hospital<-c(Hospital,stdat[1,1])
        
      }
      else if(num == "worst"){
        Hospital<-c(Hospital,stdat[nrow(stdat),1])
      }
      else{
        if(num>count){
          print(count)
          Hospital<-c(Hospital,"NA")
          #Hospital
          next
        }
        
        Hospital<-c(Hospital,stdat[num,1])
      }
      
      
    }
    
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  allranks<-data.frame(Hospital,states)
  allranks<-allranks[order(allranks$states),]
  rownames(allranks)<-allranks$states
  allranks
  
 
}
