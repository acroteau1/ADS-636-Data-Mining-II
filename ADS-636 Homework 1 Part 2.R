#######################################################
# Homework 1 Question 2
# ADS-636: Data Mining II
# Alison Croteau
# Created: 11/11/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library(rpart) # decision trees
library(dplyr) # data manipulation
library(arules) # association rules and itemsets

# Initialize workspace
graphics.off()
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Load the data
dats <- read.transactions("groceries.csv", sep = ",")
dats
summary(dats)

#######################################################
# Variable Information
# Formal class transactions in sparse format with 9835 
# transactions and 169 items. 
#######################################################

# 2a - Visualize the item frequency plot for the top 40 
# grocery items.

# Visualize the item frequency plot for the top 40 grocery items
itemFrequencyPlot(dats, topN = 40)

# 2b - Rank the top five rules with the highest “confidence”.
# Generate some association rules
my_params <- list(support = 0.005, confidence = 0.4, minlen = 2, maxlen = 6)
my_rules <- apriori(dats, parameter = my_params)

# Sort by confidence
inspect(sort(my_rules, by = "confidence")[1:5])

# 2c - Rank the top ten rules with the highest “lift”.
inspect(sort(my_rules, by = "lift")[1:10])

# 2e - pastry rules
pastryrules <- subset(my_rules, lhs %in% "pastry")
inspect(sort(pastryrules, by = "confidence"))
inspect(sort(pastryrules, by = "lift"))