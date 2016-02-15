n <- length(coordList)

newList <- data.frame()
x <- 1
y <- 1

for (i in 1:n){
  if (!is.null(coordList[[i]])) {
    
    tmp <- coordList[[i]]

    newList[i,1] <- tmp[1]
    newList[i,2] <- tmp[2]
    
  }
    
  if (x == 50000) {
    new <- x * y
    print(new)
    y <- y+1
    x <- 0
  }
  
  x <- x+1
  
}

newerList <- newList[!is.na(newList[,1]),]

newestList <- na.omit(newList)