library(dplyr)
#load("RE.rda")

#function to take cluster ids and return dataframe of number of trips
source("MappingSupportFunctions.r")
###function takes the cluster centroids as c(lat,long) and returns dataframe with trips
return_Trips<-function(centroid1,centroid2){
###Load the cluster centroid - will be from master cluster list using loop
#location1<-locations[locations$locationID == "USKZ",] #Atlanta service center - "USKZ"
#centroid1<- centroid1

#location2<-locations[locations$locationID ==6100100465,] #6100100465 - krogger
#centroid2<- c(location2$latitude,location2$longitude)

##use whInCircle function to subset the pings that have been at both clusters
whCircle1<- WhInCircle(1500,centroid1,ReducedEvents)
whCircle2<- WhInCircle(1500,centroid2,ReducedEvents)

whCircle<- whCircle1
whCircle[whCircle2]<-TRUE

combined_assets<- ReducedEvents[whCircle,]

#get only the pings between clusters and with type 3 and 4
cluster1_Assets <- ReducedEvents[whCircle1,]
cluster2_Assets <- ReducedEvents[whCircle2,]
cluster1_Assets <- cluster1_Assets[cluster1_Assets$type == 3 |cluster1_Assets$type == 4,]
cluster2_Assets <- cluster2_Assets[cluster2_Assets$type == 3 |cluster2_Assets$type == 4,]

#order pings according to date time from oldest to latest
cluster1_Assets<- cluster1_Assets[order(cluster1_Assets$datetime),]
cluster2_Assets<- cluster2_Assets[order(cluster2_Assets$datetime),]
cluster1_Assets$cluster<-1
cluster2_Assets$cluster<-2

#Fetch assets that have been to both sites
common_assets <- unique((semi_join(cluster1_Assets,cluster2_Assets,by = "assetID"))$assetID)
common_id <- unique((semi_join(cluster1_Assets,cluster2_Assets,by = "assetID"))$id)

#combine the pings
combined_pings<- rbind(cluster1_Assets,cluster2_Assets)

#dataframe to store trip details
trips = data.frame(trip_id=NA,start_id = NA, end_id = NA,assetID=NA, start_lat=NA,start_lon=NA,end_lat=NA,end_lon=NA,start_time="",end_time="")
trips$start_time = as.character(trips$start_time)
trips$end_time = as.character(trips$end_time)
trip_num=1
#finding trips between two  clusters
for (i in common_assets){
cluster_assets<-combined_pings[combined_pings$assetID == i,]
cluster_assets<-arrange(cluster_assets,datetime)
cluster_assets$datetime<-as.character(cluster_assets$datetime)
#Loop over combined set to identify trips

for (j in 1:nrow(cluster_assets)){
  if(j==1){
    prev_loc<-cluster_assets[1,]$cluster
    }
  else{
    #identify last move event from one location and first stop event at next location
    if(cluster_assets[j,]$cluster!=prev_loc && cluster_assets[j-1,]$type == 3){
      trips[trip_num,]$trip_id <- paste(c(cluster_assets[j-1,]$id,"00",trip_num),collapse = "") 
      trips[trip_num,]$start_id <- cluster_assets[j-1,]$id
      trips[trip_num,]$end_id <- cluster_assets[j,]$id
      trips[trip_num,]$assetID <- cluster_assets[j-1,]$assetID
      trips[trip_num,]$start_lat <- cluster_assets[j-1,]$latitude
      trips[trip_num,]$start_lon <- cluster_assets[j-1,]$longitude
      trips[trip_num,]$start_time <- cluster_assets[j-1,]$datetime
      trips[trip_num,]$end_lat <- cluster_assets[j,]$latitude
      trips[trip_num,]$end_lon <- cluster_assets[j,]$longitude
      trips[trip_num,]$end_time <- cluster_assets[j,]$datetime
      prev_loc <- cluster_assets[j,]$cluster
      trip_num <- trip_num+1
    }
  }
}
}
return (trips)
}