findMatchVector <- function(Input) {

  locationDB <- geoClustFinal
  
  rowID <- as.integer(Input[3])
  Input <- as.double(Input[1:2])

  a <- !is.na(match(trunc(locationDB[,1], prec = -1), trunc(Input[1], prec = -1)))
  b <- !is.na(match(trunc(locationDB[,2], prec = -1), trunc(Input[2], prec = -1)))
  c <- ((a == TRUE) & (b == TRUE))
  
#  print(sum(c))
#  print(Input)
#  print(rownames(Input))
  
  if (sum(c) > 0) {
    
    
    rowIndex <- which(c)
    
    combodata <- rbind(locationDB[rowIndex,], Input)
    
    distMatrix <- distm(cbind(combodata[2], combodata[1]), fun =distHaversine)
    
    n <- nrow(combodata)
    
    distReturn <- cbind(as.integer(rowIndex), distMatrix[n,1:(n-1)])  
    
    matchDist <- which((distReturn[,2] < matchCrit))
    
    #matchDF <- cbind(as.integer(rep(rowID, (n-1))), rowIndex[matchDist], distReturn[matchDist,2])
    matchDF <- cbind(as.integer(rep(rowID, length(matchDist))), rowIndex[matchDist], distReturn[matchDist,2])
    
    MatchMatrix <<- rbind(MatchMatrix, matchDF)
    
    return()
    
  } else {
    
    matchDF <- cbind(as.integer(rowID), NA, NA)
    MatchMatrix <<- rbind(MatchMatrix, matchDF)
    return()
    
  }

  
}