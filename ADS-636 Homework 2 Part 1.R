#######################################################
# Homework 2 Question 1
# ADS-636: Data Mining II
# Alison Croteau
# Created: 11/21/2023
# Modified: ---
#######################################################

# Install and import necessary libraries
library(recommenderlab)

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Load the data
data(MovieLense)

#######################################################
# Variable Information
# MovieLense is a realRatingMatrix of 100,000  ratings
# from 1-5 from 943 users on 1664 movies. 
# 
# MovieLenseMeta is 1664 observations of 22 variables:
# title, year, url, unknown genre, Action, Adventure,
# Animation, Children's, Comedy, Crime, Documentary,
# Drama, Fantasy, Film-Noir, Horror, Musical, Mystery,
# Romance, Sci-Fi, Thriller, War, Western.
# 
# MoveLenseUser is 943 observations of 5 variables: 
# id, age, sex, occupation, and zipcode. 
#######################################################

# Create a user-based collaborative filtering model
rec_model <- Recommender(MovieLense, method = "UBCF", parameter = list(nn = 50))

# Get the list of users and movies
users <- rownames(as(MovieLense, "matrix"))
movies <- colnames(as(MovieLense, "matrix"))

# Function to get top 10 recommendations for a user
get_top_recs <- function(user_id, k = 50, n = 10) {
  # Ensure user_id is a valid row index
  if (!user_id %in% rownames(MovieLense)) {
    stop("Invalid user ID")
  }
  
  # Convert the user's ratings into a regular numeric vector
  user_ratings <- as(as(MovieLense[user_id, ], "matrix"), "numeric")
  
  # Find k most similar users to the given user
  sim_users <- as(similarity(MovieLense[user_id, ], MovieLense, method = "cosine"), "matrix")
  sim_users_sorted <- order(sim_users, decreasing = TRUE)[1:k]
  
  # Predict ratings for movies not seen by the user
  unseen_movies <- which(is.na(user_ratings))
  pred_ratings <- sapply(unseen_movies, function(movie) {
    similar_users_ratings <- as(as(MovieLense[sim_users_sorted, movie], "matrix"), "numeric")
    mean(similar_users_ratings, na.rm = TRUE)
  })
  
  # Select top n movies with highest ratings
  top_movies <- order(pred_ratings, decreasing = TRUE)[1:min(n, length(pred_ratings))]
  colnames(MovieLense)[unseen_movies][top_movies]
}

# Test the function for three users
top_recs_1 <- get_top_recs(users[1])
top_recs_2 <- get_top_recs(users[2])
top_recs_3 <- get_top_recs(users[3])

# Print recommendations
print(top_recs_1)
print(top_recs_2)
print(top_recs_3)