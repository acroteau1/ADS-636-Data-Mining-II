#################################################################
# Homework 4 Question 7
# ADS-636: Data Mining II
# Alison Croteau
# Created: 12/21/2023
# Modified: ---
#################################################################

# Install and import necessary libraries
library(ISLR)
library(kohonen)
library(cluster)

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Import and preview the data
data(iris)
?iris

#################################################################
# Variable Information
# Data Frame with 150 observations of 5 variables
# The data set gives the measurements in centimeters of the 
# variables sepal length (Sepal.Length) and width (Sepal.Width) 
# and petal length (Petal.Length) and width (Petal.Width), 
# respectively, for 50 flowers from each of 3 species of iris. 
# The species are Iris setosa, versicolor, and virginica.
#################################################################

# Agglomerative clustering function
agglomerative_clustering <- function(dissimilarity, method = "single") {
  # Get num of observations, initialize clusters, and matrix to record merge history
  n <- nrow(dissimilarity)
  clusters <- rep(1:n, each = 1)
  history <- matrix(NA, nrow = n-1, ncol = 3)
  
  # Iteratively merge clusters
  for (step in 1:(n - 1)) {
    # Find the closest pair of clusters
    min_dist <- Inf
    # Loop over all clusters
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        # Ensure comparisons are between different clusters
        if (clusters[i] != clusters[j]) {
          dist <- dissimilarity[i, j]
          # Update minimum distance and remaining clusters
          if (dist < min_dist) {
            min_dist <- dist
            merge <- c(i, j)
          }
        }
      }
    }
    
    # Merge the closest pair of clusters and record in history matrix
    clusters[clusters == clusters[merge[2]]] <- clusters[merge[1]]
    history[step, ] <- c(clusters[merge[1]], clusters[merge[2]], min_dist)
    
    # Update the dissimilarity matrix based on the method
    if (method == "single") {
      dissimilarity[clusters == clusters[merge[1]], clusters == clusters[merge[1]]] <- Inf
      for (k in which(clusters != clusters[merge[1]])) {
        d <- min(dissimilarity[c(merge[1], merge[2]), k])
        dissimilarity[clusters[merge[1]], k] <- d
        dissimilarity[k, clusters[merge[1]]] <- d
      }
    }
    if (method == "average") {
      dissimilarity[clusters == clusters[merge[1]], clusters == clusters[merge[1]]] <- Inf
      for (k in which(clusters != clusters[merge[1]])) {
        d <- mean(dissimilarity[c(merge[1], merge[2]), k])
        dissimilarity[clusters[merge[1]], k] <- d
        dissimilarity[k, clusters[merge[1]]] <- d
      }
    }
    if (method == "complete") {
      dissimilarity[clusters == clusters[merge[1]], clusters == clusters[merge[1]]] <- Inf
      for (k in which(clusters != clusters[merge[1]])) {
        d <- max(dissimilarity[c(merge[1], merge[2]), k])
        dissimilarity[clusters[merge[1]], k] <- d
        dissimilarity[k, clusters[merge[1]]] <- d
      }
    }
  }
  
  return(list(clusters = clusters, history = history))
}

# Apply the function to the iris dataset for each method
dissimilarity <- as.matrix(dist(iris[, 1:4]))
clusters_single <- agglomerative_clustering(dissimilarity, "single")
clusters_average <- agglomerative_clustering(dissimilarity, "average")
clusters_complete <- agglomerative_clustering(dissimilarity, "complete")

# Plot the results as dendrograms for each method with clusters
single <- hclust(as.dist(dissimilarity), method = "single")
plot(single, main = "Single Linkage")
rect.hclust(single, k = 3)
average <- hclust(as.dist(dissimilarity), method = "average")
plot(average, main = "Average Linkage")
rect.hclust(average, k = 3)
complete <- hclust(as.dist(dissimilarity), method = "complete")
plot(complete, main = "Complete Linkage")
rect.hclust(complete, k = 3)