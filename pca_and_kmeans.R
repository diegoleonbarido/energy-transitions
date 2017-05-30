# https://stackoverflow.com/questions/15376075/cluster-analysis-in-r-determine-the-optimal-number-of-clusters
# https://www.r-bloggers.com/pca-and-k-means-clustering-of-delta-aircraft/

library(rql)
library(RColorBrewer)
library(scales)
library(fpc)
library(cluster)


plot(x_countries_above_2[,10:20])

princ <- prcomp(dmTrain,center = TRUE,scale. = TRUE) 

comp <- data.frame(princ$x[,1:5])

# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

# Three clusters as evaluated through the elbow method
k <- kmeans(comp,10,nstart=50,iter.max = 1000)

palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp[,1:5], col=k$clust, pch=16)

sort(table(k$clust))

clust <- names(sort(table(k$clust)))

row.names(dTrainNTreatedXscaled_dropped)

row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[1],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[2],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[3],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[4],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[5],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[6],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[7],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[8],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[9],])
row.names(dTrainNTreatedXscaled_dropped[k$clust==clust[10],])



#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- comp
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#Silhouette Score
pamk.best <- pamk(data)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(data, pamk.best$nc))














