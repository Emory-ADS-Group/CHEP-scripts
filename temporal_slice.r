source("cluster_support.r")
source("merge_support.r")

load("RE.rda")

#ReducedEvents$datetime <- strptime(ReducedEvents$datetime, format = "%Y-%m-%d %H:%M:%S")

ReducedEvents$datetime <- as.character(ReducedEvents$datetime)

randPingID <- sample(ReducedEvents$id)

numPings <- length(randPingID)
numSlices <- as.integer(ceiling(numPings/10000))
assetCRad <- 1500.0

MasterList <- list()
returnList <- list()

#for (i in 51:100) {
for (i in 1:2) {
  
  i <- 2
  #Obtain a subsample of 10,000 pings

  subSample <- obtainSubset(numPing, numSlices, i, randPingID, ReducedEvents)
  
  
  #Caluclates the initial clustering file with hclust function
  
  distout <- as.dist(distm(cbind(subSample[,5],subSample[,4]), fun=distHaversine))
  out1 <- hclust(distout)
  
  returnList <- LClust(assetCRad, subSample, out1)
  
  MasterList <- clusterEat(MasterList, returnList)

  print(i)
  
}

ConvertedList <- lapply(MasterList, centDtlConvert)