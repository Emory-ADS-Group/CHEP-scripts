findMatch <- function(locationUniq, locationDB, matchCrit) {

  a <- !is.na(match(trunc(locationDB[,1], prec = 0), trunc(locationUniq[1], prec = 0)))
  b <- !is.na(match(trunc(locationDB[,2], prec = 0), trunc(locationUniq[2], prec = 0)))
  c <- ((a == TRUE) & (b == TRUE))
  
  if (sum(c) > 0) {
    
    
    rowIndex <- which(c)
    
    combodata <- rbind(locationDB[rowIndex,], locationUniq)
    
    distMatrix <- distm(cbind(combodata[2], combodata[1]), fun =distHaversine)
    
    n <- nrow(combodata)
    
    distReturn <- cbind(as.integer(rowIndex), distMatrix[n,1:(n-1)])  
    
    matchDist <- which((distReturn[,2] < matchCrit))
    
    matchDF <- cbind(rowIndex[matchDist], distReturn[matchDist,2])
    
    #  matchDF <- distReturn[which(matchDist),]
    
    return(matchDF)
    
    
  } else {
    
    matchDF <- NA
    return(matchDF)
    
  }
  
  
  
}



# Assumptions: locationDB file will have at least 2 columns, named "Lat" and "Long"
# unique location file will have 2 columns, Lat and Long