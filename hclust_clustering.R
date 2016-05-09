library(ggplot2)
library(ggmap)
library(fields)

setwd("C:/Users/Vikaasa/IdeaProjects/Spark-Workshop")

yelp = read.csv("traindata_newfeatures.csv")

summary(yelp)

trainData <- yelp[,c("business_id","latitude","longitude")]
train=trainData[,c(2:3)]
summary(train)



lat = trainData[,c(2)]
lon = trainData[,c(3)]


mapgilbert <- get_map(location = c(lon = mean(trainData$longitude), lat = mean(trainData$latitude)), zoom = 12,
                      maptype = "hybrid", scale = 2)

threshold.in.km <- 1.60934
coors <- data.frame(lon,lat)

#distance matrix
dist <- rdist.earth(coors,miles = F,R=6371)

#clustering
set.seed(4410)
fit <- hclust(as.dist(dist), method = "complete")
clusters <- cutree(fit,h = threshold.in.km)


plot(lat, lon, col = clusters, pch = 20) 

ggmap(mapgilbert) +
  geom_point(data = trainData, aes(x = lon, y = lat, fill=clusters, alpha = 0.1), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

ggmap(mapgilbert) +
  geom_point(data=trainData, aes(x=lon, y=lat, color=factor(clusters)), size=4)+
  scale_color_discrete("Cluster")+
  coord_fixed()

clusters_toframe <-data.frame(lat,lon, clusters)
op <- data.frame(trainData[1], clusters_toframe)

write.csv(op, file = "PA_clustered.csv")

# tab=(table(lat,lon, clusters))
# out <- cbind(lat, lon, clusterNum = clusters$cluster)

