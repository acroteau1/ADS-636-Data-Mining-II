#######################################################
# Homework 2 Question 3
# ADS-636: Data Mining II
# Alison Croteau
# Created: 11/24/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library("ggplot2")
library("tidyverse")
library("fossil")
library("cluster")

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Load the data
data(iris)
?iris

## Create a plot using the first two principal components, 
## and color the iris species by class.
# Perform PCA on independent variables
iris_pca <- prcomp(iris[,1:4], center = TRUE, scale. = TRUE)

# Convert iris to data frame
iris_df <- data.frame(iris_pca$x, Species = iris$Species)

# Create the plot
ggplot(iris_df, aes(x = PC1, y = PC2, color = Species)) +
  geom_point() +
  theme_minimal() +
  labs(title = "PCA 1 and 2 of Iris Dataset", x = "Principal 
       Component 1", y = "Principal Component 2")

## Perform k-means clustering on the first two principal 
## components of the iris data. Plot the clusters different 
## colors, and then specify different symbols to depict the species labels.
# K-means clustering on the first two principal components
kmeans_clust <- kmeans(iris_pca$x[,1:2], centers = 3)

# Convert iris to data frame with cluster information
iris_k_df <- data.frame(iris_pca$x, Species = iris$Species, 
                        Cluster = kmeans_clust$cluster)

# Create the plot
ggplot(iris_k_df, aes(x = PC1, y = PC2, color = factor(Cluster), 
                      shape = Species)) +
  geom_point() +
  scale_color_manual(values = c("red", "blue", "green")) +
  theme_minimal() +
  labs(title = "Iris PC1 and PC2 K-Means Clustering", 
       x = "Principal Component 1", y = "Principal Component 2")

## Use rand index and adjusted rand index to assess how well the 
## cluster assignments capture the species labels.
rand.index(kmeans_clust$cluster, as.numeric(iris$Species))
adj.rand.index(kmeans_clust$cluster, as.numeric(iris$Species))

## Use the gap statistic and silhouette plots to determine 
## the number of clusters.
# Function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(iris_df[, 1:2], k, nstart = 20)$tot.withinss}

# Compute gap statistics
gap_kmeans <- clusGap(iris_df[, 1:2], FUN = kmeans, K.max = 10, B = 100)
plot(gap_kmeans, main = "Gap Statistic: kmeans")

# Determine the optimal number of clusters
opt_clust <- maxSE(gap_kmeans$Tab[, "gap"], gap_kmeans$Tab[, "SE.sim"])
opt_clust

# Cluster with optimal number of clusters
opt_kmeans <- kmeans(iris_df[, 1:2], centers = opt_clust)

# Calculate the silhouette
si <- silhouette(opt_kmeans$cluster, dist(iris_df[, 1:2]))

# Plot the silhouette
plot(si, col = 1:max(opt_kmeans$cluster), border = NA, main = "Silhouette Plot")