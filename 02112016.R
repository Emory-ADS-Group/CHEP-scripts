source("functions.r")

Events <- read.csv("~/Downloads/locations/Events.csv")

assetID <- unique(Events[[2]])

assetIDscram <- sample(assetID)

#Sample of the 2000 assetID's we want is in assetIDscram[1:2000]
#match(Events$assetID, assetIDscram[1:2000]) returns an interger when it is in assetIDscram
# or NA otherwise
#!is.na() returns true when it's an asset id we want to select
#wh give us the row we want to select

wh <- !is.na(match(Events$assetID, assetIDscram[1:2000]))

FirstSample <- Events[wh,]

#use the below to check that you have all the data you want - with or without sum, with and without == or !=

#sum(sort(unique(assetIDscram[1:2000])) == sort(unique(FirstSample$assetID)))
#sort(unique(assetIDscram[1:2000])) == sort(unique(FirstSample$assetID))
#sort(unique(assetIDscram[1:2000])) != sort(unique(FirstSample$assetID))

#do the two lines above for the rest of the data in order to get your 6 lists of 2000 and 1 list of 257

assetRecord <- unique(FirstSample$assetID)
numRecord <- length(assetRecord)

finalOut <- list()
x <- 1
for (x in 1:numRecord) {
  
  print(x)
  
  tmpSpace <- !is.na(match(FirstSample$assetID, assetRecord[x]))  
  tmpSample <- FirstSample[tmpSpace,]
  
  #units of distout is metre
  
  #monkey <- distm(cbind(tmpSample[,12],tmpSample[,11]), fun=distHaversine)
  #distout <- as.dist(monkey)
  
  distout <- as.dist(distm(cbind(tmpSample[,12],tmpSample[,11]), fun=distHaversine))
  
  out1 <- hclust(distout)
  # Step backwards through cuts from n to 1 until max cluster distance is too big
  #
  
  
  # Set the asset cluster radius in meters
  assetCRad <- 1000.0
  
  # Find the number of rows that we need to cycle through
  n <- nrow(tmpSample)  #finish this line of code
  upperBound <- n
  lowerBound <- 1
  
  oldUpper <- upperBound
  oldLower <- lowerBound
  
  
  maxDist <- 0
  
  while (!foundSolution) {
    
    if ((upperBound - lowerBound) == 1) {
      criticalCut <- medBound
      break
    }
    
    medBound = as.integer(round(0.5 * (upperBound + lowerBound)))
    
    maxDist <- calcMax(out1, tmpSample, medBound)
    
    if (maxDist >= assetCRad) {
      lowerBound <- medBound
    } else if (maxDist < assetCRad) {
      upperBound <- medBound
    }
    
    criticalCut <- medBound + 1
    
    
    cent <- calcCent(out1, criticalCut, tmpSample)

  }
  
  finalOut <- c(finalOut, cent)
  
}

