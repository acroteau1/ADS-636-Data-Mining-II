#######################################################
# Homework 1 Question 3
# ADS-636: Data Mining II
# Alison Croteau
# Created: 11/15/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library(dplyr) # data manipulation
library(arules) # association rules and itemsets
library(ggplot2) # visualization
library(recommenderlab)

# Initialize workspace
graphics.off()
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Load the data
housing <- read.csv("housing.csv", header = TRUE)
head(housing)
summary(housing)

#######################################################
# Variable Information
# 20640 observations of 10 variables
# longitude - num
# latitude - num
# housing_median_age - num
# total_rooms - num
# total_bedrooms - num
# population - num
# households - num
# median_income - num
# median_house_value - num
# ocean_proximity - chr
#######################################################

# Remove irrelevant variables
housing[["longitude"]] <- NULL
housing[["latitude"]] <- NULL

# Map continuous variables into suitable categories
housing[["median_income"]] <- ordered(cut(housing[["median_income"]], 
                                          c(0, 2, 4, 16)), 
                                      labels = c("Low", "Medium", "High"))
housing[["housing_median_age"]] <- ordered(cut(housing[["housing_median_age"]], 
                                          c(1, 20, 32, 52)), 
                                      labels = c("New", "Medium", "Old"))
housing[["median_house_value"]] <- ordered(cut(housing[["median_house_value"]], 
                                               c(14999, 119600, 179700, 500001)), 
                                           labels = c("Low", "Medium", "High"))
housing[["total_rooms"]] <- ordered(cut(housing[["total_rooms"]], 
                                        c(2, 1448, 3148, 39320)), 
                                    labels = c("Low", "Medium", "High"))
housing[["population"]] <- ordered(cut(housing[["population"]], 
                                       c(3, 787, 1725, 35682)), 
                                   labels = c("Low", "Medium", "High"))
housing[["households"]] <- ordered(cut(housing[["households"]], 
                                       c(1, 280, 605, 6082)), 
                                   labels = c("Low", "Medium", "High"))
housing[["total_bedrooms"]] <- ordered(cut(housing[["total_bedrooms"]], 
                                           c(1, 296, 647, 6445)), 
                                       labels = c("Low", "Medium", "High"))

# Change ocean proximity to a factor
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

# Recode the data as a binary incidence matrix by coercing to transactions
housing_bim <- as(housing, "transactions")
housing_bim
summary(housing_bim)

# See important dataset items
itemFrequencyPlot(housing_bim, support = 0.1, cex.names = 0.8)

# Visualize the matrix
image(housing_bim[1:10,], main = "Binarized Matrix")

# Generate some association rules
my_params <- list(support = 0.005, confidence = 0.4)
my_rules <- apriori(housing_bim, parameter = my_params)

# Rank the top three rules with the highest “lift”
inspect(sort(my_rules, by = "lift")[1:3])

# Rank the top four rules with the highest confidence
inspect(sort(my_rules, by = "confidence")[1:4])

# Average priced home close to the ocean
avgoceanrulespre <- subset(my_rules, lhs %in% "median_house_value=Medium")
avgoceanrules <- subset(avgoceanrulespre, lhs %in% "ocean_proximity=NEAR OCEAN")
inspect(sort(avgoceanrules, by = "confidence")[1:3])
inspect(sort(avgoceanrules, by = "lift")[1:3])

# Characteristics in a low income area
rulesPopSmall <- subset(my_rules, lhs %in% "population=Low")
inspect(sort(rulesPopSmall, by = "confidence")[1:3])
inspect(sort(rulesPopSmall, by = "lift")[1:3])