library(sqldf)
#85 elements
load("posDwell1.rda")
#posDwellData1<-PosReturnList
#85 elements
#load("posDwell2.rda")
#posDwellData2<-PosReturnList
#252 elements
#load("posDwell3.rda")
#posDwellData3<-PosReturnList
#426 elements
#load("posDwell4.rda")
#posDwellData4<-PosReturnList
#170 elements
#FinalList<-append(posDwellData1, posDwellData2)
#422 elements
#FinalList<-append(FinalList, posDwellData3)
#848 elements
#FinalList<-append(FinalList, posDwellData4)
FinalList<-PosReturnList
save("FinalList", file = "Top10Clusters.rda")

remove(DwellDets)
remove(DwellDetails)
#creating dataframe containing other Dwell details
DwellDetails<-data.frame()
#creating temporary dataframe containing other Dwell details
DwellDets<-data.frame()
#FinalList has all the cluster and dwell details (entire 100% created by concatenating the
#four files shared by Mo)
num <- length(FinalList)
rowcount = 0
rnum = 0
#num is 848
for (i in 1:num){
  t<-FinalList[[i]]
  rnum = nrow(t$dwellDetail)
  print(rnum)
  remove(DwellDets)
  DwellDets<-data.frame()
  for (k in 1: rnum){
    DwellDets[k,1] <- as.numeric(t$dwellDetail$dwell_id[k])
    DwellDets[k,2] <- t$dwellDetail$asset_id[k]
    DwellDets[k,3] <- t$dwellDetail$dwelltime[k]
    DwellDets[k,4] <- t$dwellDetail$complete[k]
    DwellDets[k,5] <- t$dwellDetail$shocks[k]
    DwellDets[k,6] <- t$dwellDetail$flips[k]
    DwellDets[k,7] <- t$dwellDetail$temp[k]
    DwellDets[k,8] <- t$CentID
    DwellDets[k,9] <- t$CentLoc[1]
    DwellDets[k,10] <- t$CentLoc[2]
    DwellDets[k,11] <- t$dwellAvg
    DwellDets[k,12] <- t$dwellStdev
    DwellDets[k,13] <- as.numeric(t$dwellCt)
    DwellDets[k,14] <- as.numeric(t$CentWgt)
    rowcount <- rowcount + 1
    #print(rowcount)
  }
#print(k)
DwellDetails <- rbind(DwellDetails, DwellDets)
}

colnames(DwellDetails)[1] <- "Dwell_ID"
colnames(DwellDetails)[2] <- "Asset_ID"
colnames(DwellDetails)[3] <- "Dwell_Time"
colnames(DwellDetails)[4] <- "Complete"
colnames(DwellDetails)[5] <- "Shocks"
colnames(DwellDetails)[6] <- "Flips"
colnames(DwellDetails)[7] <- "Temperature"
colnames(DwellDetails)[8] <- "CentID"
colnames(DwellDetails)[9] <- "CentLOC LAT"
colnames(DwellDetails)[10] <- "CentLOC LONG"
colnames(DwellDetails)[11] <- "DwellAvg"
colnames(DwellDetails)[12] <- "DwellStdev"
colnames(DwellDetails)[13] <- "DwellCount"
colnames(DwellDetails)[14] <- "CentWeight"

#Sense-check->Has the DwellDetails frame been correctly created or not?
tmpDF<-DwellDetails
str <- "select CentID, count(*) from tmpDF group by CentID"
tmp2 <- sqldf(str,stringsAsFactors = FALSE)
tmp2

plot(DwellDetails$CentWeight, DwellDetails$DwellAvg)
plot(DwellDetails$CentWeight, DwellDetails$DwellTime)

plot(DwellDetails$Dwell_Time, DwellDetails$CentWeight)

tmpStr<-DwellDetails
str <- "select CentID, 
DwellAvg,
CentWeight, 
sum(Complete) AS Complete_Trips,
sum(Shocks) AS Total_Shocks,
sum(Flips) AS Total_Flips,
sum(Temperature) AS Total_Temp,
avg(Complete) AS Avg_Complete_Trips,
avg(Shocks) AS Avg_Shocks,
avg(Flips) AS Avg_Flips,
avg(Temperature) AS Avg_Temp,
sum(Dwell_Time) AS Tot_Dwell_Time
from tmpStr 
group by CentID,
DwellAvg,
CentWeight"
tmpRes <- sqldf(str,stringsAsFactors = FALSE)
head(tmpRes)

plot(tmpRes$CentWeight, tmpRes$Complete_Trips)
plot(tmpRes$DwellAvg, tmpRes$Complete_Trips)

plot(tmpRes$CentWeight, tmpRes$Total_Shocks)
plot(tmpRes$DwellAvg, tmpRes$Avg_Shocks)

plot(tmpRes$Avg_Shocks, tmpRes$DwellAvg)

plot(tmpRes$CentWeight, tmpRes$Total_Flips)
plot(tmpRes$DwellAvg, tmpRes$Avg_Flips)
plot(tmpRes$DwellAvg, tmpRes$Total_Flips)

plot(tmpRes$DwellAvg, tmpRes$Total_Temp)
plot(tmpRes$DwellAvg, tmpRes$Avg_Temp)

hist(DwellDetails$CentWeight)
hist(DwellDetails$DwellAvg)

remove(ClusLocs)
remove(ClusLocations)
ClusLocs <- data.frame()
ClusLocations <- data.frame()

for (i in 1:num){
  t<-FinalList[[i]]
  lnum = nrow(t$LocMatch)
  print(lnum)
  remove(ClusLocs)
  ClusLocs<-data.frame()
  for (k in 1: lnum){
    ClusLocs[k,1] <- as.numeric(t$LocMatch$V1[k])
    ClusLocs[k,2] <- t$LocMatch$V2[k]
    ClusLocs[k,3] <- t$LocMatch$V3[k]
    ClusLocs[k,4] <- t$LocMatch$V4[k]
    ClusLocs[k,5] <- t$LocMatch$V5[k]
    ClusLocs[k,6] <- t$LocMatch$V6[k]
    ClusLocs[k,7] <- t$CentID
    ClusLocs[k,8] <- t$CentLoc[1]
    ClusLocs[k,9] <- t$CentLoc[2]
    ClusLocs[k,10] <- t$dwellAvg
    ClusLocs[k,11] <- t$dwellStdev
    ClusLocs[k,12] <- as.numeric(t$dwellCt)
    ClusLocs[k,13] <- as.numeric(t$CentWgt)
    rowcount <- rowcount + 1
    #print(rowcount)
  }
  #print(k)
  ClusLocations <- rbind(ClusLocations, ClusLocs)
}

colnames(ClusLocations)[1] <- "LocationName"
colnames(ClusLocations)[2] <- "Zipcode"
colnames(ClusLocations)[3] <- "Status"
colnames(ClusLocations)[4] <- "ReturnLoc1"
colnames(ClusLocations)[5] <- "ReturnLoc2"
colnames(ClusLocations)[6] <- "custom3"
colnames(ClusLocations)[7] <- "CentID"
colnames(ClusLocations)[8] <- "CentLOC LAT"
colnames(ClusLocations)[9] <- "CentLOC LONG"
colnames(ClusLocations)[10] <- "DwellAvg"
colnames(ClusLocations)[11] <- "DwellStdev"
colnames(ClusLocations)[12] <- "DwellCount"
colnames(ClusLocations)[13] <- "CentWeight"

head(ClusLocations)
#Sense-check->Has the ClusLocations frame been correctly created or not?
tmpDF<-ClusLocations
str <- "select CentID, count(*) from tmpDF group by CentID"
tmp2 <- sqldf(str,stringsAsFactors = FALSE)
tmp2

plot(ClusLocations$Status, ClusLocations$DwellAvg)

plot(ClusLocations$custom3, ClusLocations$DwellAvg)

plot(ClusLocations$Zipcode, ClusLocations$DwellAvg)

plot(ClusLocations$LocationName, ClusLocations$DwellAvg)

hist(ClusLocations$CentWeight)
hist(ClusLocations$DwellAvg)

#Checking the frequency distribution of categorical variables
library(MASS)                 # load the MASS package 
custom3 = ClusLocations$custom3
custom3.freq = table(custom3)
custom3.freq

#Merged_Data <- merge(DwellDetails, ClusLocations, by=c("CentIID"))

