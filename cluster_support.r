LClust <- function(criticalDist, subSample, out1) {
  
  CriticalCut <- calcCC(criticalDist, subSample, out1) 
  
  criticalOut <- CriticalCut$criticalOut
  numCut <- CriticalCut$numCut
  
  centProp <- calcCP(subSample, numCut, criticalOut)
  
  return(centProp)
  
}


# This function calculates the critical cuts required to get the radius <= critical radius

calcCC <- function(criticalDist, tmpSample, out1) {
  
  n <- nrow(tmpSample)
  upperBound <- n
  lowerBound <- 1
  
  oldUpper <- upperBound
  oldLower <- lowerBound
  
  foundSolution <- FALSE
  
  maxDist <- 0

    while (!foundSolution) {
    
    if ((upperBound - lowerBound) == 1) {
      criticalCut <- medBound
      break
    }
    
    medBound = as.integer(round(0.5 * (upperBound + lowerBound)))
    
    returnMax <- calcMax(out1, tmpSample, medBound)
    maxDist <- returnMax$maxDist
    
    
    if (maxDist >= criticalDist) {
      lowerBound <- medBound
    } else if (maxDist < criticalDist) {
      upperBound <- medBound
    }
    
    criticalCut <- medBound + 1
    
  }
  
  criticalCut <- list("numCut" = criticalCut, "criticalOut" = returnMax$out)
  
  return(criticalCut)
}



#########Function calcMax: returns maxDist, takes in out1, tmpSample, and k as input################

calcMax <- function(out1, tmpSample, numGroup) {
  
  out2 <- cutree(out1, k=numGroup)
  
  maxDist <- 0
  
  for (j in 1:numGroup){ #j = cluster index produced by cutree (this loop steps through all the clusters)
    
    wh <- out2 == j #assumes vector order doesn't change
    
    if (sum(wh) > 0) {
      tmpCoord <- cbind(tmpSample[wh,5],tmpSample[wh,4])
      dst <- max(distm((tmpCoord), fun=distHaversine))
    }
    
    if (dst > maxDist) maxDist <- dst
  }
  
  returnList <- list("maxDist" = maxDist, "out" = out2)

  return(returnList)
  
}


calcCP <- function(subSample, numCut, criticalOut) {
  
  propReturn <- list()
  
  for (i in 1:numCut) {
    
    wh <- criticalOut == i
    #wh <- criticalOut == 8
    
    tmpSample <- subSample[wh,]

    #tmpSample <- tmpSample[-c(3)]
    
    
    if (sum(wh) >= 10){
      centID <- tmpSample[1,1]
      centLocation <- c(mean(tmpSample[,4]),mean(tmpSample[,5]))
      #centLocation <- c(mean(tmpSample[,3]),mean(tmpSample[,4]))   # assumes linear approximation given small range
      centWeight <- as.integer(sum(wh))
      
      #tmpSample[,3] <- as.POSIXlt(tmpSample[,3])
      centDetail <- tmpSample[,1:5]
      #centDetail <- tmpSample[,1:4]

      propReturn[[i]] <- list(
        "CentID" = centID,
        "CentLoc" = centLocation,
        "CentWgt" = centWeight,
        "CentDtl" = centDetail
        #"CentDtl" = as.data.frame(centDetail)
      )
      
    } #closes if 
    
  } #closes for
  
  return(rmNullObs(propReturn))
  
}




##########   Sets the upper and lower boundary for the main loop ##############


obtainSubset <- function(numPing, numSlices, i, randPingID, ReducedEvents) { 
  
  j <- i * 10000
  k <- j - 9999
  
  if (j > length(randPingID)) {j <- length(randPingID)}
  
  wh <- !is.na(match(ReducedEvents$id, randPingID[k:j]))
  
  return(ReducedEvents[wh,])
  
}

####### Function to remove null http://stackoverflow.com/questions/26539441/r-remove-null-elements-from-list-of-lists

## A helper function that tests whether an object is either NULL _or_ 
## a list of NULLs
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

## Recursively step down into list, removing all such objects 
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}