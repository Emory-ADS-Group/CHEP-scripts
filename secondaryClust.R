load("coordf.rda")

source("secondaryfunctions.r")

library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

orderClust <- newerList[order(newerList$Long, newerList$Lat),]

secondaryOut <- list()

ptm <- proc.time()

numRecord <- nrow(orderClust)

clustStep <- 10000
n <- 1
lower <- 1
upper <- (n * clustStep)

while (upper <= numRecord) {
  tmpSample <- orderClust[lower:upper,]
  
  print(lower)
  print(upper)
  print("---------")
  
  distout <- as.dist(distm(cbind(tmpSample[,2], tmpSample[,1]), fun=distHaversine))
  
  out1 <- hclust(distout)
  
  a <- secondaryClust(500.0, tmpSample, out1)
  
  secondaryOut <- c(secondaryOut, a)
  
  lower <- upper + 1
  n <- n + 1
  
  if (upper == numRecord) {
    break
  } else {
    upper <- n * clustStep
    if (upper > numRecord) {
      upper <- numRecord
    }
  } 
}

proc.time() - ptm