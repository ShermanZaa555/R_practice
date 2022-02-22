# Load iris dataset
str(iris)

# See header of iris dataset.
head(iris)

# Plot the relation of Petal.Length and Petal.Width .
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, col=Species)) + geom_point() + ggtitle("TechVidvan iris scatter plot")

# Remove target from dataset.
species <- as.list(iris$Species)
species <- unlist(species)
iris_df <- iris[1:4]
iris_df

# Use k-Means clustering by number of clusters is 3 and number of iterations to 20.
irisCluster <- kmeans(iris_df, 3, nstart = 20)
irisCluster

# Watch cluster size.
table(irisCluster$cluster)

# Cluster centers (Centroids).
irisCluster$centers

# Evaluate model with original dataset.
table(irisCluster$cluster, iris$Species)

# Change irisCluster to factor variable.
irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris_df, aes(Petal.Length, Petal.Width, color=irisCluster$cluster)) + geom_point() +
  ggtitle("TechVidvan iris cluster plot")

# Combine the results of clustering analysis with the original dataset.
iris.cluster = cbind(iris_df, irisCluster[1])
iris.cluster