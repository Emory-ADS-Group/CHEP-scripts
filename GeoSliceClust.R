Events <- read.csv("~/Downloads/locations/Events.csv")

sortedEvents <- Events[order(Events$longitude, Events$latitude),]

library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

source("functions.r")


geoClust <- list()

ptm <- proc.time()

numRecord <- nrow(Events)

clustStep <- 10000
n <- 1
lower <- 1
upper <- (n * clustStep)

while (upper <= numRecord) {

  tmpSample <- sortedEvents[lower:upper,11:12]
  
  print(upper)

  distout <- as.dist(distm(cbind(tmpSample[,2], tmpSample[,1]), fun=distHaversine))
  
  out1 <- hclust(distout)
  
  wh <- Cluster(1000.0, tmpSample, out1)
  
  geoClust <- c(geoClust, wh)
  
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

save(geoClust, file = "geoClust.rda", compress = "xz")

proc.time() - ptm