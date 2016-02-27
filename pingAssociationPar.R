#Loads the resource files: RE.rda, a reduced "Events" table by dropping uncessary columns, function source file and
#the previous geoClustFinal output file

library(parallel)

source("findMatchVector.r")
load("RE.rda")
load("geoClustFinal.rda")

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type="FORK")

#A random sample of AssetIDs

assetIDList <- unique(ReducedEvents$assetID)
assetIDList <- sample(assetIDList)


ptm <- proc.time()


#Setup an empty dataframe as output from the match function
MatchMatrix <- data.frame()

#This counter j, as well as codes from line 41-46, are used as a counter and not necessary for this script to function properly
j <- 0

for (i in 1:1000) {
  
  #Grabs the data from the ith asset
  wh <- !is.na(match(ReducedEvents$assetID, assetIDList[i]))
  FirstSample <- ReducedEvents[wh,]
  
  #Setup the input data frame in the format of Lat, Long, and pingID
  FirstSampleTest <- subset(FirstSample, select = c("id", "latitude", "longitude"))
  FirstSampleTest <- FirstSampleTest[,c(2, 3, 1)]
  
  #define match criterion in meters. All clusters within this distance will be returned
  matchCrit <- 2000.0
  
  #Function for vectorized match function
  parApply(cl, FirstSampleTest, 1, findMatchVector)

  #For loop counter so I know the progress
  if (j == 50) {
    print(i)
    j <- 0
  }
  
  j <- j + 1

  }

proc.time() - ptm

stopCluster(cl)
