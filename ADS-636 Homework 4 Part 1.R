#################################################################
# Homework 4 Question 1
# ADS-636: Data Mining II
# Alison Croteau
# Created: 12/18/2023
# Modified: ---
#################################################################

# Install and import necessary libraries
library(igraph)
library(igraphdata)

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Import and preview the data
data(karate)
data(kite)
?karate
?kite
summary(karate)
summary(kite)

#################################################################
# Variable Information
# Karate - List of 10
# Social network between members of a university karate club.
# Kite - List of 10
# An undirected igraph graph with graph attributes name, 
# layout, Citation, Author, URL, and vertex attributes 
# label, name and Firstname.
#################################################################

### Karate Network
# Create a noisy dataset by removing 5% of the edges randomly
set.seed(123)
n_edges <- gsize(karate)
n_remove <- round(n_edges * 0.05)
edges_to_remove <- sample(E(karate), n_remove, replace = FALSE)
plot(karate, main = "karate - original")

# Store original edges for later comparison
original_edges <- E(karate)
karate_noisy <- delete_edges(karate, edges_to_remove)
plot(karate_noisy, main = "karate - noisy")

# Fit Hierarchical Random Graphs
ghrg <- fit_hrg(karate_noisy)
plot_dendrogram(ghrg, main = "Hierarchical Random Graph") # FIXME: title not working?

# Predict missing edges
pred <- predict_edges(karate_noisy)

# Add some predicted edges to the graph in a different color
E(karate_noisy)$color <- "blue"
lay <- layout_nicely(karate_noisy)
karate_pred <- add_edges(karate_noisy, t(pred$edges[1:n_remove, ]), color = "red")
plot(karate_pred, layout = lay, main = "Karate - noisy with predicted edges")

### Kite Network
# Create a noisy dataset by removing 5% of the edges randomly
set.seed(123)
n_edges_kite <- gsize(kite)
n_remove_kite <- round(n_edges_kite * 0.05)
edges_to_remove_kite <- sample(E(kite), n_remove_kite, replace = FALSE)
plot(kite, main = "kite - original")

# Store original edges for later comparison
original_edges_kite <- E(kite)
kite_noisy <- delete_edges(kite, edges_to_remove_kite)
plot(kite_noisy, main = "kite - noisy")

# Fit Hierarchical Random Graphs
ghrg_kite <- fit_hrg(kite_noisy)
plot_dendrogram(ghrg_kite, main = "Hierarchical Random Graph") # FIXME: title not working?

# Predict missing edges
pred_kite <- predict_edges(kite_noisy)

# Add some predicted edges to the graph in a different color
E(kite_noisy)$color <- "blue"
lay_kite <- layout_nicely(kite_noisy)
kite_pred <- add_edges(kite_noisy, t(pred_kite$edges[1:n_remove_kite, ]), color = "red")
plot(kite_pred, layout = lay_kite, main = "Kite - noisy with predicted edges")

### Repeat with 15% and 40% removal
n_remove_15 <- round(n_edges * 0.15)
edges_to_remove_15 <- sample(E(karate), n_remove_15, replace = FALSE)
n_remove_40 <- round(n_edges * 0.40)
edges_to_remove_40 <- sample(E(karate), n_remove_40, replace = FALSE)
n_remove_15_kite <- round(n_edges_kite * 0.15)
edges_to_remove_15_kite <- sample(E(kite), n_remove_15_kite, replace = FALSE)
n_remove_40_kite <- round(n_edges_kite * 0.40)
edges_to_remove_40_kite <- sample(E(kite), n_remove_40_kite, replace = FALSE)

# Store original edges for later comparison
karate_noisy_15 <- delete_edges(karate, edges_to_remove_15)
plot(karate_noisy_15, main = "karate - 15% noisy")
karate_noisy_40 <- delete_edges(karate, edges_to_remove_40)
plot(karate_noisy_40, main = "karate - 40% noisy")
kite_noisy_15 <- delete_edges(kite, edges_to_remove_15_kite)
plot(kite_noisy_15, main = "kite - 15% noisy")
kite_noisy_40 <- delete_edges(kite, edges_to_remove_40_kite)
plot(kite_noisy_40, main = "kite - 40% noisy")

# Fit Hierarchical Random Graphs
ghrg_15 <- fit_hrg(karate_noisy_15)
plot_dendrogram(ghrg_15, main = "Hierarchical Random Graph - 15%")
ghrg_40 <- fit_hrg(karate_noisy_40)
plot_dendrogram(ghrg_40, main = "Hierarchical Random Graph - 40%")
ghrg_15_kite <- fit_hrg(kite_noisy_15)
plot_dendrogram(ghrg_15_kite, main = "Hierarchical Random Graph - 15%")
ghrg_40_kite <- fit_hrg(kite_noisy_40)
plot_dendrogram(ghrg_40_kite, main = "Hierarchical Random Graph - 40%")

# Predict missing edges
pred_15 <- predict_edges(karate_noisy_15)
pred_40 <- predict_edges(karate_noisy_40)
pred_15_kite <- predict_edges(kite_noisy_15)
pred_40_kite <- predict_edges(kite_noisy_40)

# Add some predicted edges to the graph in a different color
E(karate_noisy_15)$color <- "blue"
lay_15 <- layout_nicely(karate_noisy_15)
karate_pred_15 <- add_edges(karate_noisy_15, t(pred_15$edges[1:n_remove_15, ]), color = "red")
plot(karate_pred_15, layout = lay_15, main = "Karate - noisy with pred 15%")
E(karate_noisy_40)$color <- "blue"
lay_40 <- layout_nicely(karate_noisy_40)
karate_pred_40 <- add_edges(karate_noisy_40, t(pred_40$edges[1:n_remove_40, ]), color = "red")
plot(karate_pred_40, layout = lay_40, main = "Karate - noisy with pred 40%")
E(kite_noisy_15)$color <- "blue"
lay_15_kite <- layout_nicely(kite_noisy_15)
kite_pred_15 <- add_edges(kite_noisy_15, t(pred_15_kite$edges[1:n_remove_15_kite, ]), color = "red")
plot(kite_pred_15, layout = lay_15_kite, main = "Kite - noisy with pred 15%")
E(kite_noisy_40)$color <- "blue"
lay_40_kite <- layout_nicely(kite_noisy_40)
kite_pred_40 <- add_edges(kite_noisy_40, t(pred_40_kite$edges[1:n_remove_40_kite, ]), color = "red")
plot(kite_pred_40, layout = lay_40_kite, main = "Kite - noisy with pred 40%")