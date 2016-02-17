Events <- read.csv("~/Downloads/locations/Events.csv")

sortedEvents <- Events[order(Events$longitude, Events$latitude),]

library("geosphere", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

source("functions.r")

data1 <- data.frame(1:800)

data1 <- t(data1)

data2 <- sample(data1, 30, replace = FALSE)

geoClust <- list()

#ptm <- proc.time()

for (i in 10:30) {
  
  lower <- data2[i] * 10000
  upper <- lower + 10000
  
  if (upper<=nrow(Events)) {
    
    tmpSample <- sortedEvents[lower:upper,11:12]
    
    print(lower)
    
    distout <- as.dist(distm(cbind(tmpSample[,2], tmpSample[,1]), fun=distHaversine))
    
    out1 <- hclust(distout)
    
    wh <- Cluster(1000.0, tmpSample, out1)
    
    geoClust <- c(geoClust, wh)
    
  }
  
  
}



proc.time() - ptm