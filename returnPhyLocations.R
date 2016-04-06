# NOTE: Cluster should be in the format  c(lat.,Long.), Locations is the csv file, MatchCrit is in meter 
# the variables for output dataframe is c("name","zipcode","chepStatus","lat.","long.","classification"))
  
 returnPhyLocations<- function(Cluster, Locations, MatchCrit){
   #Prepration:import locations.csv  Load geosphere package, load findMatch.r file with following syntax:
   
   #source("findMatch.r")
   
   #clean up the locations.cvs 
   Clusterdata <- data.frame()
   Clusterdata[1,1]<-Cluster[[1]]
   Clusterdata[1,2]<-Cluster[[2]]
   
   LocationDF<- data.frame(Locations,stringsAsFactors=F)
  
   data1 <-unique.matrix(LocationDF[c("latitude","longitude","name","zipcode","chepStatus","custom3")])
   
   Newthing <- na.omit(data1)
   colnames(Newthing)<-c("V1","V2","name","zipcode","chepStatus","custom3") 
   NewRoundLocation <- Newthing[c("V1","V2")]
   NewLocations  <- data.frame()
   NewLocations <- matrix(ncol=6,nrow=0)
   Outlist <- data.frame(matrix(ncol=6))
   
   # 1. Declares the locationDB file  and for loop to identify 
   
   locationDB <-  NewRoundLocation
   locationUniq <- Clusterdata 
   locationMatch <- findMatch(locationUniq, locationDB,  MatchCrit)
     if (is.na(locationMatch[1])){
       Outlist <- NA 
     }
     else {
       FinalIndex <- locationMatch[,1]
       Outlist<- matrix(nrow=length(FinalIndex),ncol=6)
       
       ReturnLoc <- locationDB [FinalIndex,]
       Outlist [,1]<-as.character( Newthing $name[FinalIndex])
       Outlist [,2]<- as.character(  Newthing $zipcode[FinalIndex])
       Outlist [,3]<- as.character( Newthing$chepStatus[FinalIndex])
       Outlist [,4]<- ReturnLoc$V1
       Outlist [,5]<- ReturnLoc$V2
       Outlist [,6]<- as.character(  Newthing$custom3[FinalIndex])
       outlist <-as.data.frame(Outlist)
       #make.names(c("name","zipcode","chepStatus","lat.","long.","classification"))
   }
  }
   
   
   
   
   
   
   
   
   
   
   
