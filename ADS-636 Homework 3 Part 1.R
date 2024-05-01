#######################################################
# Homework 3 Question 1
# ADS-636: Data Mining II
# Alison Croteau
# Created: 12/6/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library(ISLR)
library(kohonen)

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Load and preview the data
data(USArrests)
head(USArrests)
?USArrests

#######################################################
# Variable Information
# Violent Crime Rates by US State
# 50 observations of 4 variables
# Murder - numeric murder arrests per 100,000
# Assault - integer assault arrests per 100,000
# UrbanPop - integer percent urban population
# Rape - numeric rape arrests per 100,000
#######################################################

# Calculate HC Distance
dist_matrix <- dist(USArrests, method = "euclidean")
hc <- hclust(dist_matrix, method = "complete")

# Plot the results
plot(hc, hang = -1, labels = rownames(USArrests))

# Checking dendrogram for various k values
# rect.hclust(hc, k = 2) Too few clusters
rect.hclust(hc, k = 3) # Good number of clusters
# rect.hclust(hc, k = 4) Splits become too specific
# rect.hclust(hc, k = 5)

# Scale the data
arr_scaled <- scale(USArrests)

# Fit a Self Organizing Map
set.seed(123)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
arr_som <- som(arr_scaled, grid = som_grid, rlen = 3000)
codes <- arr_som$codes[[1]]

# Plot the data
plot(arr_som, main = "Arrest Data")
plot(arr_som, type = "changes", main = "Arrest Data")
plot(arr_som, type = "count")
plot(arr_som, type = "mapping")
coolBlueHotRed <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

# Hierarchical Clustering on SOM Codebook Vectors
dm_som <- dist(codes)
hc_som <- hclust(dm_som, method = "complete")
plot(hc_som)  # Plot the dendrogram
rect.hclust(hc_som, k = 3)  # Drawing rectangles for k=3 clusters

# U-matrix visualization
plot(arr_som, type = "dist.neighbours", main = "U-Matrix", 
     palette.name = coolBlueHotRed)