#################################################################
# Homework 3 Question 4
# ADS-636: Data Mining II
# Alison Croteau
# Created: 12/13/2023
# Modified: ---
#################################################################

# Install and import necessary libraries
library(ggplot2)

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Import and preview the data
load("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636\\SwissBankNotes-2.rdata")
head(SwissBankNotes)
str(SwissBankNotes)

#################################################################
# Variable Information
# 200 observations of 6 numeric variables
# Obs. 1-100 are genuine, 101-200 are counterfeit
# length - length of banknote
# height.left - height, measured on the left
# height.right - height, measured on the right
# inner.lower - distance of the inner frame to the lower border
# inner.upper - distance of the inner frame to the upper border
# diagonal - length of the diagonal
#################################################################

# Create data sets based on genuine and counterfeit observations
genuine <- SwissBankNotes[1:100, ]
counterfeit <- SwissBankNotes[101:200, ]

# Perform PCA on genuine data set
pca_genuine <- prcomp(genuine, scale = TRUE)
pca_genuine_summary <- summary(pca_genuine)
var_genuine_explained <- pca_genuine_summary$importance[2,]
pca_genuine_df <- data.frame(PC = 1:length(var_genuine_explained), 
                             Variance = var_genuine_explained)

# Perform PCA on counterfeit data set
pca_counterfeit <- prcomp(counterfeit, scale = TRUE)
pca_counterfeit_summary <- summary(pca_counterfeit)
var_counterfeit_explained <- pca_counterfeit_summary$importance[2,]
pca_counterfeit_df <- data.frame(PC = 1:length(var_counterfeit_explained), 
                             Variance = var_counterfeit_explained)

# Perform PCA on combined data set
pca_combined <- prcomp(SwissBankNotes, scale = TRUE)
pca_combined_summary <- summary(pca_combined)
var_combined_explained <- pca_combined_summary$importance[2,]
pca_combined_df <- data.frame(PC = 1:length(var_combined_explained), 
                                 Variance = var_combined_explained)

# Create a scree plot for genuine data
ggplot(pca_genuine_df, aes(x = PC, y = Variance)) + geom_line() + 
  geom_point(size = 3) + labs(title = "Genuine Data Scree Plot", 
                              x = "Principal Component", 
                              y = "Variance Explained") + theme_minimal()

# Create a scree plot for counterfeit data
ggplot(pca_counterfeit_df, aes(x = PC, y = Variance)) + geom_line() + 
  geom_point(size = 3) + labs(title = "Counterfeit Data Scree Plot", 
                              x = "Principal Component", 
                              y = "Variance Explained") + theme_minimal()

# Create a scree plot for combined data
ggplot(pca_combined_df, aes(x = PC, y = Variance)) + geom_line() + 
  geom_point(size = 3) + labs(title = "Combined Data Scree Plot", 
                              x = "Principal Component", 
                              y = "Variance Explained") + theme_minimal()

# Prepare data for ggplot
genuine_data <- as.data.frame(pca_genuine$x)
genuine_data$type <- "Genuine"

counterfeit_data <- as.data.frame(pca_counterfeit$x)
counterfeit_data$type <- "Counterfeit"

combined_data <- as.data.frame(pca_combined$x)
combined_data$type <- ifelse(1:200 <= 100, "Genuine", "Counterfeit")

# Score plot for the genuine data
ggplot(genuine_data, aes(PC1, PC2)) +
  geom_point() +
  ggtitle("PCA Score Plot - Genuine Data") +
  theme_minimal()

# Score plot for the counterfeit data
ggplot(counterfeit_data, aes(PC1, PC2)) +
  geom_point() +
  ggtitle("PCA Score Plot - Counterfeit Data") +
  theme_minimal()

# Score plot for combined data
ggplot(combined_data, aes(PC1, PC2, color = type)) +
  geom_point() +
  ggtitle("PCA Score Plot - Combined Data") +
  theme_minimal()