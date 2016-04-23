#function to calculate dwell time at each cluster
source("MappingSupportFunctions.r")

return_dwell<-function(centroid,radius){
whAsset<- WhInCircle(centroid,radius,ReducedEvents)
assets<-ReducedEvents[whAsset,]


unique_assets<-unique(assets$assetID)
dwells<-data.frame(dwell_id=NA,asset_id=NA,dwelltime=NA,complete=NA,shocks=NA,flips=NA,temp=NA)
start<-proc.time()
dwell_count<-1
for (i in unique_assets){
  current_asset<-ReducedEvents[ReducedEvents$assetID == i,]
  whCurrent<-WhInCircle(centroid,radius,current_asset)
  current_asset$atLoc<-whCurrent
  current_asset<-current_asset[order(current_asset$datetime),]
  current_asset$datetime <-as.character(current_asset$datetime)
  current_asset$datetime<-strftime(current_asset$datetime,format = "%Y-%m-%d %H:%M:%S")
  first_true<-1 
  if(nrow(current_asset)<2){
    dwells[dwell_count,]$dwell_id<-paste(c(current_asset[1,]$id,dwell_count),collapse = "")
    dwells[dwell_count,]$asset_id<-i
    dwells[dwell_count,]$dwelltime<-0
    dwells[dwell_count,]$complete<-0
    dwells[dwell_count,]$shocks<-current_asset[1,]$countShock
    dwells[dwell_count,]$flips<-current_asset[1,]$countFlip
    dwells[dwell_count,]$temp<-current_asset[1,]$temperature
    first_true<-1
    dwell_count<-dwell_count+1
  }else{
  for (j in 2:nrow(current_asset)){
    
      if(current_asset[j-1,]$atLoc ==TRUE){
          first_true<-first_true+1
          if(current_asset[j,]$atLoc ==FALSE){
            last_true<-j-1
            if(first_true<last_true){
            first_true<-last_true - first_true
            }else{
              first_true=last_true
            }
            current_dwell<-current_asset[first_true:last_true,]
            
            dwells[dwell_count,]$dwell_id<-paste(c(current_asset[j-1,]$id,dwell_count),collapse = "")
            dwells[dwell_count,]$asset_id<-i
            dwells[dwell_count,]$dwelltime<-difftime(current_dwell[nrow(current_dwell),]$datetime,current_dwell[1,]$datetime,units="secs")
            dwells[dwell_count,]$complete<-1
            dwells[dwell_count,]$shocks<-sum(current_dwell$countShock)
            dwells[dwell_count,]$flips<-sum(current_dwell$countFlip)
            dwells[dwell_count,]$temp<-mean(current_dwell$temperature)
            first_true<-1
            dwell_count<-dwell_count+1
            }
      }
  }
  }
}
end<-proc.time()
end-start
return(dwells)
}




