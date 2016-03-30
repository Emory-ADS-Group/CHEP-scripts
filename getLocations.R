    #Prepration: Load geosphere package, load findMatch.r file with following syntax:
    
    # getLocations takes one unique physical location identified by CHEP,our identified cluster nodes and  radius (MatchCrit) for geofence  
    # to print out all of satisified nodes matched up CHEP location within the circle ident
    
    # NOTE: PhysicalLoc should be in the format  c(lat.,Long.), MaterList is our list of clustered nodes, MatchCrit is in meter 


  getLocations <- function(PhysicalLoc, MasterList, MatchCrit){
      Physicaldata <- data.frame()
      Physicaldata[1,1]<-PhysicalLoc[[1]]
      Physicaldata[1,2]<-PhysicalLoc[[2]]
      
      NewLocations  <- data.frame()
      NewLocations <- matrix(ncol=3,nrow=0)
      Outlist <- list()
      Masterdf <- data.frame(matrix(ncol=2,nrow=0))
      for (i in 1:length(MasterList)){ 
        Addr <- matrix(nrow=1,ncol=2)
        row.names(Addr)<- MasterList[[i]]$CentID
        Addr[1]<- MasterList[[i]]$CentLoc[1]
        Addr[2]<- MasterList[[i]]$CentLoc[2]
        Masterdf <- rbind(Masterdf, Addr) 
   }
    
      
      locationDB <- Masterdf
      locationUniq <- Physicaldata
      locationMatch <- findMatch(locationUniq, locationDB, MatchCrit)
        if (is.na(locationMatch[1])){
          Outlist<- NA 
       
        }
        else {
          FinalIndex <- locationMatch[,1]
          AddLocations <- matrix(nrow=length(FinalIndex),ncol=3)
          
          ReturnLoc <- locationDB [FinalIndex,]
        
         
          AddLocations[,1]<- row.names(ReturnLoc)
          AddLocations[,2]<-ReturnLoc$V1
          AddLocations[,3]<-ReturnLoc$V2
          
          NewLocations <- rbind(NewLocations,AddLocations)
          Outlist <-list(AddLocations)
         
        }
}

  # Our outlist shows all of matched clusterID, cluster node latitue and cluster node longitude.

      
      
