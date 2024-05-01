#######################################################
# Homework 2 Question 2
# ADS-636: Data Mining II
# Alison Croteau
# Created: 11/21/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library("recommenderlab")
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")
library("corrplot")
library("tidyverse")

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Load the data
data(state)
?state
state.x77

#######################################################
# Variable Information
# Data sets related to the 50 states of the United 
# States of America. 
# state.abb - 2-letter abbreviation of state name
# state.area - numeric vector of state area (sq. miles)
# state.center - list with components named x and y 
# giving the approximate geographic center of each state 
# in negative longitude and latitude.
# state.division - factor giving state division (New 
# England, Middle Atlantic, South Atlantic, East South 
# Central, West South Central, East North Central, West 
# North Central, Mountain, and Pacific).
# state.name - character vector of full state name
# state.region - factor of region for each state (Northeast,
# South, North Central, West). 
# state.x77 - matrix with 50 rows and 8 columns giving the 
# following statistics in the respective columns. Population: 
# population estimate. Income: per capita income. Illiteracy:
# illiteracy (percent of population). Life Exp: life expectancy 
# in years. Murder: murder and non-negligent manslaughter rate 
# per 100,000 population. HS Grad: percent high-school graduates.
# Frost: mean number of days with minimum temperature below 
# freezing (1931–1960) in capital or large city.  Area: land 
# area in square miles.
#######################################################

# Focus on the data Population, Income, Illiteracy, Life Exp, 
# Murder, HS Grad, Frost, Area. Cluster this data (states) 
# using hierarchical clustering. Keep the class labels (region, 
# or state name) in mind, but do not use them in the modeling. 

# HC for overall data
d_states <- dist(state.x77)
dim(as.matrix(d_states))
hc_states <- hclust(d_states, method = "ave")
plot(hc_states)

# Create a correlation matrix to shift focus to variables
state_cor <- cor(state.x77)
state_cor

# Some visualizations to understand the correlations
pairs(state_cor)
corrplot(state_cor)

# Calculate the distance for HC
d <- dist(state_cor)
dim(as.matrix(d))
hc <- hclust(d, method = "ave")

# Visualization of the HC
plot(hc)

# Cut the tree into groups and compare
ct <- cutree(hc, k = c(1,5))
table(grp1 = ct[,"1"], grp5 = ct[,"5"])
ct <- cutree(hc, k = 5)

# View silhouette
si <- silhouette(ct, dist = d)
plot(si)

# Concat the regions to the state.x77 matrix
region <- as.data.frame(state.region)
state.x77 <- cbind(state.x77, region)

# Check concat worked correctly
head(state.x77)

# Plot some region comparisons to see murder 
# rate and life expectancy by region
data <- as.data.frame(state.x77)
g1 <- ggplot(data, aes(x = data$state.region, y = data$Murder)) +
  geom_bar(stat = "identity") + theme_minimal() +
  labs(title = "Murder rate per state region", x = "Region",
       y = "Murder rate")
plot(g1)

g2 <- ggplot(data, aes(x = data$state.region, y = data$`Life Exp`)) +
  geom_bar(stat = "identity") + theme_minimal() +
  labs(title = "Life Expectancy per state region", x = "Region",
       y = "Life Expectancy")
plot(g2)