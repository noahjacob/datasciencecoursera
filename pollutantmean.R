
pollutantmean <- function(directory, pollutant, id = 1:332){
  files <- list.files(path = directory)
  #gets the files names in a vector
  
  means<-c()
  
  for(i in id){
    data <- read.csv(file.path(directory,files[i]))
    int_dat <- data[pollutant]
    int_dat <- int_dat[!is.na(int_dat)]
    means<-c(means,int_dat)
    #print(nrow(data))
    
  }
  print(mean(means))
 
}

complete <- function(directory,id = 1:332){
  files <- list.files(path = directory)
  results <- data.frame(id=numeric(0), nobs=numeric(0))
  
  for(i in id){
    data <- read.csv(file.path(directory,files[i]))
    int_dat <- data[complete.cases(data),]
    
    nobs <- nrow(int_dat)
    results <- rbind(results,data.frame(id = i, nobs = nobs ))
    
  }
  
  return(results)
}

