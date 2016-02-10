#########Function calcMax: returns maxDist, takes in out1, tmpSample, and k as input################

calcMax <- function(out1, tmpSample, numGroup) {
  
  out2 <- cutree(out1, k=numGroup)
  
  maxDist <- 0
  
  for (j in 1:numGroup){ #j = cluster index produced by cutree (this loop steps through all the clusters)
    wh <- out2 == j #assumes vector order doesn't change
    if (sum(wh) < 1) stop("No ping in this cluster")
    
    tmpCoord <- cbind(tmpSample[wh,12],tmpSample[wh,11])
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
      cent[i] <- list(c(mean(tmpSample[wh,11]),mean(tmpSample[wh,12])))
    } 
  }
  
  return(cent)
  
}