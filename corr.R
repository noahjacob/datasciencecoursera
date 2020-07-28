corr <- function(directory,threshold = 0){
  results<-data.frame( id = numeric(0), nobs = numeric(0))
  
  results<-complete(directory)
  
  new_res<-results[results$nobs>=threshold,]
  #print(new_res)
  ids<-c()
  ids<-c(ids,new_res$id)
  
  if(length(ids)>0){
  files<-list.files(path = directory)
   cor_res<-numeric(0)
  for(i in ids){
    dat <- read.csv(file.path(directory,files[i]))
    int_dat<-dat[complete.cases(dat),]
    sul_dat<-int_dat$sulfate
    nit_dat<-int_dat$nitrate
    cor_res<-c(cor_res,cor(sul_dat,nit_dat))
    
  }
  return(cor_res)
  }
  else{
     return(0)
  }
  
}##length gives wrong output for some reason
