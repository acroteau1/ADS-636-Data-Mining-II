#######################################################
# Homework 3 Question 3
# ADS-636: Data Mining II
# Alison Croteau
# Created: 12/10/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library(corrplot)
library(ggplot2)
# install.packages("factoextra")
library(factoextra)
library(cluster)
library(kohonen)

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

### Self-Organizing Map

# Combine both wine datasets into one matrix
wine_red$type <- 'Red'
wine_wht$type <- 'White'
wine_comb <- rbind(wine_red, wine_wht)

# Create new DF with just numeric variables for SOM
wine_comb_som <- wine_comb[,1:11]

# Scale the data
wine_scaled <- scale(wine_comb_som)

# Fit a Self Organizing Map
set.seed(123)
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
wine_som <- som(wine_scaled, grid = som_grid, rlen = 3000)
codes <- wine_som$codes[[1]]

# Plot the SOM
plot(wine_som, type = "mapping")

# Create clusters
text(wine_som$grid$pts, labels = wine_som$unit.classif, cex = 1.5)
hc <- hclust(dist(codes))
ct <- cutree(hc, 5)
plot(ct)
plot(wine_som, type = "codes", bgcol = rainbow(5)[ct])

# Cluster boundaries
add.cluster.boundaries(wine_som, ct)

# Add a column in your combined dataset to represent colors for each type
wine_comb$color <- ifelse(wine_comb$type == 'Red', 'red', 'blue')

# Create a vector to store the predominant color for each SOM unit
predominant_color <- rep(NA, length(unique(wine_som$unit.classif)))

# Loop through each unit and determine the predominant wine type
for (unit in unique(wine_som$unit.classif)) {
  samples_in_unit <- which(wine_som$unit.classif == unit)
  types_in_unit <- wine_comb$type[samples_in_unit]
  predominant_type <- names(which.max(table(types_in_unit)))
  predominant_color[unit] <- ifelse(predominant_type == 'Red', 'red', 'blue')
}

# Plot the SOM and color the samples based on wine type
plot(wine_som, type = "mapping", col = predominant_color[wine_som$unit.classif], pch = 20)
legend("topright", legend = c("Red", "White"), col = c("red", "blue"), pch = 20)

### Phase Plots

# Loop through each variable and create a phase-plot
for (i in 1:ncol(wine_scaled)) {
  plot(wine_som, type = "property", property = wine_scaled[,i], 
       main = colnames(wine_scaled)[i])
}

# Plotting prototype vector information on the SOM
plot(wine_som, type="codes")