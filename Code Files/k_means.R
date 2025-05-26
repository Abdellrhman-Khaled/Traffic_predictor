# working directory
setwd("C:/Users/salma/OneDrive/Desktop/uni/year 4/semester 2/big data")

trafficDF <- read.csv("Traffic_dataset_with_temperature_and_visibility.csv")

install.packages("factoextra")

library(factoextra)

# description el kmeans mn el slides: 
# The K-Means function, provided by the cluster package, is used as follows:

#kmeans (x, centers, iter.max = 10, nstart = 1, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))

#• x: A numeric matrix of data, or an object that can be coerced to such a matrix
#(such as a numeric vector or a data frame with all numeric columns).
#• centers: Either the number of clusters or a set of initial (distinct) cluster centers.
#If a number, a random set of (distinct) rows in x is chosen as the initial centers.
#• iter.max: The maximum number of iterations allowed.
#• nstart: If centers is a number, nstart gives the number of random sets that
#should be chosen.
#• algorithm: The algorithm to be used. It should be one of the following "Hartigan-
#Wong", "Lloyd", "Forgy" or "MacQueen". If no algorithm is specified, the
#algorithm of Hartigan and Wong is used by default.

# features in traffic dataset:
# 1- time -> converted to sine and cosine to be cyclical
# 2- days of the week -> could be converted to also be cyclical but according to 
#our EDA and visualization, days of the week did not significantly affect traffic 
#so theres no need to include it here
# 3- total count is enough 
# 4- temp
# 5- visibility 

# Load and parse time -> AI generated,, da 3shan n make time cyclical 
trafficDF$Time <- strptime(trafficDF$Time, format = "%I:%M:%S %p")

# Convert to decimal hour
hours <- as.numeric(format(trafficDF$Time, "%H")) + 
  as.numeric(format(trafficDF$Time, "%M")) / 60

# Cyclical encoding
trafficDF$Time_sin <- sin(2 * pi * hours / 24)
trafficDF$Time_cos <- cos(2 * pi * hours / 24)

k_means_features <- trafficDF[,c("Time_sin","Time_cos","Total","Temperature", "Visibility.in.metres")]

#copy your dataset without the class column
newTraffic <- trafficDF

#set column value to NULL
newTraffic$Traffic.Situation <- NULL

set.seed(123) # for reproducibility 3shan a3ml kda tany for el documentation
kmeans_traffic <- kmeans(k_means_features,4)

kmeans_traffic

kmeans_traffic$cluster

kmeans_traffic$centers

#before seeing the table to compare el kmeans gab ad eh sah
# number of heavy traffic situations: 
sum(trafficDF$Traffic.Situation == "heavy")
# = 682

# number of high traffic situations: 
sum(trafficDF$Traffic.Situation == "high")
# = 321

# number of low traffic situations: 
sum(trafficDF$Traffic.Situation == "low")
# = 304 

# number of normal traffic situations: 
sum(trafficDF$Traffic.Situation == "normal")
# = 1669

table(trafficDF$Traffic.Situation,kmeans_traffic$cluster)
# cluster 1 -> heavy
# cluster 2 -> normal
# cluster 3 -> low
# cluster 4 -> high


#Silhouette values range from -1 to 1:
#Close to 1: Data points are well matched to their own cluster and poorly matched to neighboring clusters (ideal clustering).
#Around 0: Data points are on or very close to the decision boundary between clusters.
#Less than 0: Data points may be assigned to the wrong cluster.

library(cluster)
sil <- silhouette(kmeans_traffic$cluster, dist(k_means_features))
mean_sil <- mean(sil[, 3])
cat("Average silhouette width:", mean_sil, "\n")

