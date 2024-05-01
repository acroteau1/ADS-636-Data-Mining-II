#######################################################
# Homework 3 Question 2
# ADS-636: Data Mining II
# Alison Croteau
# Created: 12/8/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library(corrplot)
library(ggplot2)
# install.packages("factoextra")
library(factoextra)
library(cluster)

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Import and preview the data
wine_red <- read.csv("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636\\winequality-red.csv", 
                     sep=';')
wine_wht <- read.csv("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636\\winequality-white.csv", 
                     sep=';')
head(wine_red)
str(wine_red)
head(wine_wht)
str(wine_wht)

#######################################################
# Variable Information
# The two datasets are related to red and white variants
# of the Portuguese "Vinho Verde" wine.
# Red - 1599 observations of 12 variables
# White - 4898 observations of 12 variables
# Variables 1-11 are numeric variables
# 1 - fixed acidity
# 2 - volatile acidity
# 3 - citric acid
# 4 - residual sugar
# 5 - chlorides
# 6 - free sulfur dioxide
# 7 - total sulfur dioxide
# 8 - density
# 9 - pH
# 10 - sulphates
# 11 - alcohol
# 12 - quality (int score between 0 and 10)
#######################################################

### Exploratory Data Analysis

# View statistical summaries and boxplots of the data
summary(wine_red)
summary(wine_wht)
boxplot(wine_red$fixed.acidity, wine_wht$fixed.acidity, 
        main = "fixed acidity", names = c("Red Wine", "White Wine"))
boxplot(wine_red$volatile.acidity, wine_wht$volatile.acidity, 
        main = "volatile acidity", names = c("Red Wine", "White Wine"))
boxplot(wine_red$citric.acid, wine_wht$citric.acid, 
        main = "citric acid", names = c("Red Wine", "White Wine"))
boxplot(wine_red$residual.sugar, wine_wht$residual.sugar, 
        main = "residual sugar", names = c("Red Wine", "White Wine"))
boxplot(wine_red$chlorides, wine_wht$chlorides, 
        main = "chlorides", names = c("Red Wine", "White Wine"))
boxplot(wine_red$free.sulfur.dioxide, wine_wht$free.sulfur.dioxide, 
        main = "free sulfur dioxide", names = c("Red Wine", "White Wine"))
boxplot(wine_red$total.sulfur.dioxide, wine_wht$total.sulfur.dioxide,
        main = "total sulfur dioxide", names = c("Red Wine", "White Wine"))
boxplot(wine_red$density, wine_wht$density, 
        main = "total sulfur dioxide", names = c("Red Wine", "White Wine"))
boxplot(wine_red$pH, wine_wht$pH, 
        main = "pH", names = c("Red Wine", "White Wine"))
boxplot(wine_red$sulphates, wine_wht$sulphates, 
        main = "sulphates", names = c("Red Wine", "White Wine"))
boxplot(wine_red$alcohol, wine_wht$alcohol, 
        main = "alcohol", names = c("Red Wine", "White Wine"))
boxplot(wine_red$quality, wine_wht$quality, 
        main = "quality", names = c("Red Wine", "White Wine"))

# Explore correlations between numeric variables
red <- cor(wine_red[,1:11])
corrplot(red, method="circle")

# Combine both wine datasets into one matrix
wine_red$type <- 'Red'
wine_wht$type <- 'White'
wine_comb <- rbind(wine_red, wine_wht)

# Comparative plot for wine type
ggplot(wine_comb, aes(x=quality, fill=type)) + 
  geom_histogram(binwidth=1, position="dodge")

### PCA and Red Wine

# Prepare numeric variables only and scale
wine_red_pca <- wine_red[,1:11]
wine_red_pca <- scale(wine_red_pca)

# Perform PCA
pca_red <- prcomp(wine_red_pca, center = TRUE, scale. = TRUE)
pca_red_summary <- summary(pca_red)
var_red_explained <- pca_red_summary$importance[2,]
pca_red_df <- data.frame(PC = 1:length(var_red_explained), Variance = var_red_explained)

# Create a biplot
fviz_pca_biplot(pca_red)
biplot(pca_red)

# Create a scree plot
ggplot(pca_red_df, aes(x = PC, y = Variance)) + geom_line() + 
  geom_point(size = 3) + labs(title = "Scree Plot", x = "Principal Component", 
                              y = "Proportion of Variance Explained") + theme_minimal()

### PCA and White Wine

# Prepare numeric variables only and scale
wine_wht_pca <- wine_wht[,1:11]
wine_wht_pca <- scale(wine_red_pca)

# Perform PCA
pca_wht <- prcomp(wine_wht_pca, center = TRUE, scale. = TRUE)
pca_wht_summary <- summary(pca_wht)
var_wht_explained <- pca_wht_summary$importance[2,]
pca_wht_df <- data.frame(PC = 1:length(var_wht_explained), Variance = var_wht_explained)

# Create a biplot
fviz_pca_biplot(pca_wht)
biplot(pca_wht)

# Create a scree plot
ggplot(pca_wht_df, aes(x = PC, y = Variance)) + geom_line() + 
  geom_point(size = 3) + labs(title = "Scree Plot", x = "Principal Component", 
                              y = "Proportion of Variance Explained") + theme_minimal()

### Combined data PCA

# Prepare numeric variables only and scale
wine_comb_pca <- wine_comb[,1:11]
wine_comb_pca <- scale(wine_comb_pca)

# Perform PCA
pca_comb <- prcomp(wine_comb_pca, center = TRUE, scale. = TRUE)
pca_comb_summary <- summary(pca_comb)
var_comb_explained <- pca_comb_summary$importance[2,]
pca_comb_df <- data.frame(PC = 1:length(var_comb_explained), Variance = var_comb_explained)

# Create a biplot
fviz_pca_biplot(pca_comb)
biplot(pca_comb)

# Create a scree plot
ggplot(pca_comb_df, aes(x = PC, y = Variance)) + geom_line() + 
  geom_point(size = 3) + labs(title = "Scree Plot", x = "Principal Component", 
                              y = "Proportion of Variance Explained") + theme_minimal()

### K-means using silhouette method

# Create cluster variables
wine_red_cluster <- wine_red[, 1:11]
wine_wht_cluster <- wine_wht[, 1:11]

# Determine number of clusters using silhouette
sil_width_red <- rep(0, 15)

for (i in 2:15) {
  sil_width_red[i] <- silhouette(kmeans(wine_red_cluster, centers=i)$cluster, 
                             dist(wine_red_cluster))[, 3]
}

plot(1:15, sil_width_red, type="b", xlab="Number of Clusters", ylab="Average Silhouette Width", 
     main = "Silhouette width per number of clusters for red wine")

sil_width_wht <- rep(0, 15)

for (i in 2:15) {
  sil_width_wht[i] <- silhouette(kmeans(wine_wht_cluster, centers=i)$cluster, 
                             dist(wine_wht_cluster))[, 3]
}

plot(1:15, sil_width_wht, type="b", xlab="Number of Clusters", ylab="Average Silhouette Width", 
     main = "Silhouette width per number of clusters for white wine")

# Ideal k=2 for red wine, and k=2 for white wine
# Perform k-means clustering
set.seed(123) 
k <- 2
wine_red_kmeans <- kmeans(wine_red_cluster, centers=k)
wine_wht_kmeans <- kmeans(wine_wht_cluster, centers=k)

# Cluster means
wine_red_kmeans$centers
wine_wht_kmeans$centers

# Cluster sizes
wine_red_kmeans$size
wine_wht_kmeans$size

### Perform k-means using Principal Components

# PCA already performed on variables pca_red and pca_wht
# Review scree plots for number of PC's
# Extract the principal components
wine_red_pc <- pca_red$x[, 1:2]
wine_wht_pc <-pca_wht$x[, 1:2]

# Utilizing silhouette method again for ideal k
sil_width_red <- rep(0, 15)

for (i in 2:15) {
  clustering_red <- kmeans(wine_red_pc, centers=i)
  silhouette_scores_red <- silhouette(clustering_red$cluster, dist(wine_red_pc))
  sil_width_red[i] <- mean(silhouette_scores_red[, "sil_width"])
}

plot(1:15, sil_width_red, type="b", xlab="Number of Clusters", ylab="Average Silhouette Width", 
     main = "Silhouette width per number of clusters for red wine")

sil_width_wht <- rep(0, 15)

for (i in 2:15) {
  clustering_wht <- kmeans(wine_wht_pc, centers=i)
  silhouette_scores_wht <- silhouette(clustering_wht$cluster, dist(wine_wht_pc))
  sil_width_wht[i] <- mean(silhouette_scores_wht[, "sil_width"])
}

plot(1:15, sil_width_wht, type="b", xlab="Number of Clusters", ylab="Average Silhouette Width", 
     main = "Silhouette width per number of clusters for white wine")

# Perform k-means on principal components
set.seed(123)
k <- 2
wine_red_kmeans_pc <- kmeans(wine_red_pc, centers=k)
wine_wht_kmeans_pc <- kmeans(wine_wht_pc, centers=k)

# Cluster centers
wine_red_kmeans_pc$centers
wine_wht_kmeans_pc$centers

# Cluster sizes
wine_red_kmeans_pc$size
wine_wht_kmeans_pc$size