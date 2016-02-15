#########Function calcMax: returns maxDist, takes in out1, tmpSample, and k as input################

calcMax <- function(out1, tmpSample, numGroup) {
  
  out2 <- cutree(out1, k=numGroup)
  
  maxDist <- 0
  
  for (j in 1:numGroup){ #j = cluster index produced by cutree (this loop steps through all the clusters)
    wh <- out2 == j #assumes vector order doesn't change
    if (sum(wh) < 1) stop("No ping in this cluster")
    
    tmpCoord <- cbind(tmpSample[wh,2],tmpSample[wh,1])
    dst <- max(distm((tmpCoord), fun=distHaversine))
    
    
    if (dst > maxDist) maxDist <- dst
  }
  
  return(maxDist)
  
}


calcCent <- function(out1, criticalCut, tmpSample) {
  
  out3 <- cutree (out1, k=criticalCut)
  
  cent <- list()
  
  for (i in 1:criticalCut) {
    wh <- out3 == i
    if (sum(wh) > 1){
      cent[i] <- list(c(mean(tmpSample[wh,1]),mean(tmpSample[wh,2])))
    } 
  }
  
  return(cent)
  
}

secondaryClust <- function(criticalDist, tmpSample, out1) {

  # Find the number of rows that we need to cycle through
  n <- nrow(tmpSample)  #finish this line of code
  upperBound <- n
  lowerBound <- 1
  
  oldUpper <- upperBound
  oldLower <- lowerBound
  
  foundSolution <- FALSE
  
  maxDist <- 0
  
  #criticalDist <- 500.0
  
  
  while (!foundSolution) {
    
    if ((upperBound - lowerBound) == 1) {
      criticalCut <- medBound
      break
    }
    
    medBound = as.integer(round(0.5 * (upperBound + lowerBound)))
    
    maxDist <- calcMax(out1, tmpSample, medBound)
    
    if (maxDist >= criticalDist) {
      lowerBound <- medBound
    } else if (maxDist < criticalDist) {
      upperBound <- medBound
    }
    
    criticalCut <- medBound + 1
    
    
    cent <- calcCent(out1, criticalCut, tmpSample)
  }
  
  return(cent)
  
}