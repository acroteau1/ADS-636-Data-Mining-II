#######################################################
# Homework 1 Question 1
# ADS-636: Data Mining II
# Alison Croteau
# Created: 11/11/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library(rpart) # decision trees
library(dplyr) # data manipulation

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Import the downloaded data file "marketing.rdata"
load("marketing.RData")
head(marketing)

#######################################################
# Variable Information
# 8993 observations of 14 variables
# All variables are integers
#
# Income - Annual income of household, categorical, 
# scale of 1 to 9 where 1 (Less than $10,000), 2 ($10,000 
# to $14,999), 3 ($15,000 to $19,999), 4 ($20,000 to 
# $24,999), 5 ($25,000 to $29,999), 6 ($30,000 to $39,999),
# 7 ($40,000 to $49,999), 8 ($50,000 to $74,999), 9 
# ($75,000 or more)
#
# Sex - 1 (female) or 2 (male)
#
# Marital - Marital status: 1 (Married), 2 (Living 
# together, not married), 3 (Divorced or separated),
# 4 (Widowed), 5 (Single, never married)
#
# Age - age where 1 (14 thru 17), 2 (18 thru 24), 3 (25 
# thru 34), 4 (35 thru 44), 5 (45 thru 54), 6 (55 thru 
# 64), 7 (65 and Over)
# 
# Edu - 1 (Grade 8 or less), 2 (Grades 9 to 11), 3 
# (Graduated high school), 4 (1 to 3 years of college), 
# 5 (College graduate), 6 (Grad Study)
# 
# Occupation - 1 (Professional/Managerial), 2 (Sales 
# Worker), 3 (Factory Worker/Laborer/Driver), 4 
# (Clerical/Service Worker), 5 (Homemaker), 6 (Student, 
# HS or College), 7 (Military), 8 (Retired), 9 (Unemployed)
# 
# Lived - 1 (Less than one year), 2 (One to three years), 3 
# (Four to six years), 4 (Seven to ten years), 5 (More than 
# ten years)
# 
# Dual_Income - 1 (Not Married), 2 (Yes), 3 (No)
# 
# Household - 1 (One), 2 (Two), 3 (Three), 4 (Four), 5 
# (Five), 6 (Six), 7 (Seven), 8 (Eight), 9 (Nine or more)
# 
# Householdu18 - 0 (None), 1 (One), 2 (Two), 3 (Three), 4
# (Four), 5 (Five), 6 (Six), 7 (Seven), 8 (Eight), 9 (Nine 
# or more)
# 
# Status - 1 (Own), 2 (Rent), 3 (Live with Parents/Family)
# 
# Home_Type - 1 (House), 2 (Condominium), 3 (Apartment), 4 
# (Mobile Home), 5 (Other)
# 
# Ethnic - 1 (American Indian), 2 (Asian), 3 (Black), 4 
# (East Indian), 5 (Hispanic), 6 (Pacific Islander), 7 
# (White), 8 (Other)
# 
# Language -  1 (English), 2 (Spanish), 3 (Other)
# 
# The missing value flag is NA.
#######################################################

# Prepare the reference sample by permuting columns
set.seed(123) 
marketing_ref <- marketing

# Shuffle each column
for (i in 1:ncol(marketing_ref)) {
  marketing_ref[[i]] <- sample(marketing_ref[[i]])
}
head(marketing_ref)

# Build the classification tree for training (class 1)
tree_model_class1 <- rpart(Income ~ ., data = marketing)

# Print the tree summary to view the structure
printcp(tree_model_class1)

# View a summary of the nodes
summary(tree_model_class1)

# Prune the tree back
min_cp = which.min(tree_model_class1$cptable[,4])
x11()
plot(tree_model_class1$cptable[,4], main = "Cp for model selection", ylab = "cv error")

# Model the pruned tree
pruned_fit_class1 <- prune(tree_model_class1, cp = tree_model_class1$cptable[min_cp, 1])
x11()
plot(pruned_fit_class1)
text(pruned_fit_class1, use.n = TRUE, cex = .5)

# View summaries
printcp(pruned_fit_class1)
summary(pruned_fit_class1)

# Build the classification tree for reference (class 0)
combined_data <- rbind(marketing, marketing_ref)
tree_model_ref <- rpart(Income ~ ., data = combined_data)

# Print the tree summary to view the structure
printcp(tree_model_ref)

# View a summary of the nodes
summary(tree_model_ref)

# Prune the tree back
min_cp = which.min(tree_model_ref$cptable[,4])
x11()
plot(tree_model_ref$cptable[,4], main = "Cp for model selection", ylab = "cv error")

# Model the pruned tree
pruned_fit_ref <- prune(tree_model_ref, cp = tree_model_ref$cptable[min_cp, 1])
x11()
plot(pruned_fit_ref)
text(pruned_fit_ref, use.n = TRUE, cex = .5)

# View summaries
printcp(pruned_fit_ref)
summary(pruned_fit_ref)