str(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, col=Species)) + geom_point() + ggtitle("TechVidvan iris scatter plot")

species <- as.list(iris$Species)
species <- unlist(species)
iris_df <- iris[1:4]
iris_df

irisCluster <- kmeans(iris_df, 3, nstart = 20)
irisCluster

table(irisCluster$cluster)

irisCluster$centers

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris_df, aes(Petal.Length, Petal.Width, color=irisCluster$cluster)) + geom_point() +
  ggtitle("TechVidvan iris cluster plot")

iris.cluster = cbind(iris_df, irisCluster[1])
iris.cluster