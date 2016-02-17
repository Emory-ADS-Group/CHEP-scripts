outputConvert <- function(clusterOut) {
  
  
  n <- length(clusterOut)
  
  newList <- data.frame()
  x <- 1
  y <- 1
  
  for (i in 1:n){
    if (!is.null(clusterOut[[i]])) {
      
      tmp <- clusterOut[[i]]
      
      newList[i,1] <- tmp[1]
      newList[i,2] <- tmp[2]
      newList[i,3] <- as.integer(tmp[3])
    }
    
    if (x == 50000) {
      new <- x * y
      print(new)
      y <- y+1
      x <- 0
    }
    
    x <- x+1
    
  }
  
  finalData <- newList[!is.na(newList[,1]),]
  
  return(finalData)
  
}