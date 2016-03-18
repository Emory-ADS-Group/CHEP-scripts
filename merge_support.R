#Flunction clusterEat: merges 2 list files. One is destinated as masterlist, other is a newly returned list

clusterEat <- function(MasterList, returnList) {

  #Check to make sure that MasterList is a valid list, otherwise imply makes the returned list the master list
  if (length(MasterList)>0) {
    
    #Extracts the centroid location of the 2 lists and identifies the distances between cents
    centMaster <- sapply(MasterList, with, CentLoc)
    centReturn <- sapply(returnList, with, CentLoc)
    
    distmMatrix <- distm(cbind(centMaster[2,], centMaster[1,]),
                         cbind(centReturn[2,], centReturn[1,]), fun = distHaversine)
    
    minDist <- apply(distmMatrix, 2, minLookup)  #function minLookup returns the location of shortest 
                                                 #distances within the distmMatrix. Function is implemeneted
                                                 #further down file
    
    foo <- length(minDist)
    switchMatrix <- logical(foo)
    
    #For distances shorter than a critical value (50m in this case), clusterMerge function is called which
    #returns the merged cluster j from MasterList and i from returnList
    
    for (i in 1:foo) {
      
      j <- minDist[i]
      
      if (distmMatrix[j,i] <= 50.0) {
        
        MasterList[[j]] <- clusterMerge(MasterList, returnList, i, j)
        
        switchMatrix[i] <- TRUE
      }
      
      
    }
    returnList[switchMatrix] <- NULL    #Removes the now "eaten" list of centroids from the return list
                                        #and appends the remaining centroids to the MasterList
    
    MasterList <- append(MasterList, returnList)
  } else {MasterList <- returnList}
  
  return(MasterList)
  

}

#This function takes as input 2 centroids: jth list from MasterList and ith list from returnList
#and merges them by:
# 1. Using existing CentID from MasterList
# 2. Calculating weight average of the centroids
# 3. Summing the weight of the centroids
# 4. Combining the centroid details

clusterMerge <- function(MasterList, returnList, i, j) {
  
  existingCent <- MasterList[[j]]
  newCent <- returnList[[i]]
  
  newcentID <- existingCent$CentID
  
  sumWeight <- (existingCent$CentWgt + newCent$CentWgt)
  newcentLocation <- (1 / sumWeight * (existingCent$CentWgt * existingCent$CentLoc + newCent$CentWgt * newCent$CentLoc)) 
    
    
  newcentWeight <- as.integer(sumWeight)
  
  newcentDetail <- mapply(c, existingCent$CentDtl, newCent$CentDtl, SIMPLIFY=FALSE)
  
  updatedCent <- list(
    "CentID" = newcentID,
    "CentLoc" = newcentLocation,
    "CentWgt" = newcentWeight,
    "CentDtl" = newcentDetail
  )
  
  return(updatedCent)
  
}



#Find the location of the minimum distance to a cluster

minLookup <- function (x) {
  return(order(x)[1])
}

centDtlConvert <- function(tmpList) {
  
  #tmpList <- MasterList[[1]]
  
  CentDtl <- tmpList$CentDtl
  
  df <- data.frame(sapply(CentDtl,c))
  
  df$id <- as.integer(as.character(df$id))
  df$assetID <- as.integer(as.character(df$assetID))
  df$datetime <- strptime(df$datetime, format = "%Y-%m-%d %H:%M:%S")
  df$latitude <- as.double(as.character(df$latitude))
  df$longitude <- as.double(as.character(df$longitude))
  
  tmpList$CentDtl <- df
  
  return(tmpList) 
}