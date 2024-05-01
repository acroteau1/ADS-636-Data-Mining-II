#################################################################
# Homework 4 Question 6
# ADS-636: Data Mining II
# Alison Croteau
# Created: 12/20/2023
# Modified: ---
#################################################################

# Install and import necessary libraries
library(glasso)
library(gRbase)
library(gRim)
library(gRain)
library(corrplot)
library(igraph)
library(kohonen)

# Initialize workspace
rm(list = ls())
setwd("C:\\Users\\acrot\\OneDrive\\Documents\\ADS-636")

# Import and preview the data
data(state)
?state
graphics.off()

#################################################################
# Variable Information
# Data sets related to the 50 states of the USA.
# state.abb - character vector of 2-letter abbreviations for 
# the state names.
# state.area - numeric vector of state areas (in square miles).
# state.center - list with components named x and y giving the 
# approximate geographic center of each state in negative longitude 
# and latitude. 
# state.division - factor giving state divisions (New England, 
# Middle Atlantic, South Atlantic, East South Central, West South 
# Central, East North Central, West North Central, Mountain, and 
# Pacific).
# state.name - character vector giving the full state names.
# state.region - factor giving the region (Northeast, South, 
# North Central, West) that each state belongs to.
# state.x77 - matrix with 50 rows and 8 columns giving the 
# following statistics in the respective columns:
# Population - population estimate as of July 1, 1975
# Income - per capita income (1974)
# Illiteracy - illiteracy (1970, percent of population)
# Life Exp - life expectancy in years (1969–71)
# Murder - murder and non-negligent manslaughter rate per 
# 100,000 population (1976)
# HS Grad - percent high-school graduates (1970)
# Frost - mean number of days with minimum temperature below 
# freezing (1931–1960) in capital or large city
# Area - land area in square miles
#################################################################

# View a correlation matrix
M <- cor(state.x77)
corrplot(M)

# Prepare data
dats <- state.x77[ , -1]
new_dats <- dats[-c(39,42), ]

# Compute partial correlation
S.body <- cov.wt(new_dats, method = "ML")
S <- S.body$cov

# Fit the model using Graphical Lasso with multiple penalties
my_rhos <- c(2,5,10,15,25,50)
graph_list <- list()

for (rho in my_rhos) {
  fit <- glasso(S, rho = rho)
  edges <- fit$wi != 0
  diag(edges) <- 0
  g <- graph_from_adjacency_matrix(edges, mode = "undirected", diag = FALSE)
  V(g)$name <- colnames(S)  # Assign column names of S as vertex names
  graph_list[[as.character(rho)]] <- g
}

# Plot and analyze the graphs
pdf("GGM_graphs.pdf")
for (i in 1:length(graph_list)) {
  plot(graph_list[[i]], main = paste("Graphical Lasso with rho =", names(graph_list)[i]))
}
dev.off()

# Scale the data for SOM
data <- scale(new_dats)

# Define SOM grid size
som_grid <- somgrid(xdim = 5, ydim = 5, topo = "rectangular")

# Train SOM model
som_model <- som(data, grid = som_grid, rlen = 100, alpha = c(0.05, 0.01))

# Plot the SOMs
plot(som_model, type = "codes", main = "SOM Codes")
plot(som_model, type = "count", main = "SOM Count")
plot(som_model, type = "mapping", main = "SOM Mapping")