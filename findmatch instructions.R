# Prepration: Load geosphere package, load findMatch.r file with following syntax:

source("findMatch.r")

# 1. Declares a sample unique location file

locationUniq <- data.frame()
locationUniq[1,1] <- 35.01
locationUniq[1,2] <- 90.05

# 2. Declare a random cluster node file, or use one of the "cluster of cluster" files

locationDB <- cbind(runif(5000, 34.0, 36.0), runif(5000, 89.0, 91.0))


#Pass these 3 variables into findMatch: 
#1) locationUniq (vector, Lat / Long)
#2) a list of cluster nodes (2 columns, Lat / Long)
#3) a critical distance in meters

locationMatch <- findMatch(locationUniq, locationDB, 5000.0)
