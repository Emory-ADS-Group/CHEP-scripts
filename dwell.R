#function to calculate dwell time at each cluster
source("MappingSupportFunctions.r")

return_dwell<-function(centroid){
whAsset<- WhInCircle(1500,c(37.72862,-121.5278),ReducedEvents)
assets<-ReducedEvents[whAsset,]


unique_assets<-unique(assets$assetID)
dwells<-data.frame(dwell_id=NA,asset_id=NA,dwelltime=NA)
start<-proc.time()
dwell_count<-1
for (i in unique_assets){
  current_asset<-ReducedEvents[ReducedEvents$assetID == i,]
  whCurrent<-WhInCircle(1500,c(37.72862,-121.5278),current_asset)
  current_asset$atLoc<-whCurrent
  current_asset<-current_asset[order(current_asset$datetime),]
  current_asset$datetime <-as.character(current_asset$datetime)
  current_asset$datetime<-strftime(current_asset$datetime,format = "%Y-%m-%d %H:%M:%S")
  first_true<-1 
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
            first_true<-1
            dwell_count<-dwell_count+1
            }
      }
  }
  }
end<-proc.time()
end-start
}




