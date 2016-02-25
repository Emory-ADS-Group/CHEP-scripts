library(ggmap)
library(ggplot2)
library(RgoogleMaps)

# Clear out the workspace so that we do not accidentally use old stuff.

remove(list=objects())

Events <- read.csv("geoClustFinal.csv", 
                   header=TRUE, 
                   stringsAsFactors=T, 
                   strip.white=TRUE, 
                   na.strings=c("NA", "na","nan", "inf", "", "."))

ClusDF <- data.frame(Events,stringsAsFactors=F)
names(ClusDF)

colnames(ClusDF)[2] <- "ClusLatitude"
colnames(ClusDF)[3] <- "ClusLongitude"

# Use get_map( ) to get a google map. Pick the point whose longigute and latitutde are
# mid way between the largest and smallest longitudes and lattitudes in the cluster data set. 
MapLoc <- c(mean(range(ClusDF$ClusLongitude)),mean(range(ClusDF$ClusLatitude)))

# Get the map from google centered at that location.
TheRasterMapObject <- get_map(location=MapLoc,zoom=4)

# At zoom=4, you get a plot of the whole US. 
# Create the plot with the function ggmap
MyMap <- ggmap(TheRasterMapObject)

# Plot it
MyMap

# Get the list of the unique cluster IDs
colnames(ClusDF)[1] <- "ClusterID"
ClusIDList <- unique(ClusDF$ClusterID)

Count_ClusID <- length(ClusDF$ClusterID) #30 Unique Cluss
ClusIDList <- unique(ClusDF$ClusterID)

# for (i in 1:Count_ClusID)
# {
#   ClusterID = ClusIDList[i]
#   wh <- ClusterID == ClusDF$ClusterID
#   OneClusDF <- ClusDF[wh,]
# 
#   MyMap <- MyMap + geom_point(aes(x=ClusLongitude,y=ClusLatitude),color="red",
#                               size=1.0,data=OneClusDF)
#   MyMap
# }

MyMap <- MyMap + geom_point(aes(x=ClusLongitude,y=ClusLatitude),color="red",
                            size=1.0,data=ClusDF)
MyMap
ggsave(paste("Final_Map",".jpg",sep=""),MyMap,dpi=1200)
