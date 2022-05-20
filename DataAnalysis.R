# DataAnalysis.R 
# This file seeks to analyze the pattern language research in two sections
# 1 - Graphing and measuring the patterns
#     This uses the entire sample of cases (N = 86)
# 2 - Graphing and measuring the network of relationships between patterns
#     This uses the training set of cases (N=20)
# Created by: Katelyn Stenger
# Created on: March 11, 2022
# Last Edit: April 14, 2022

#########################################
# Preprocessing
# Clearing workspace
rm(list = ls())


# Set working directory
setwd("~/Library/CloudStorage/Box-Box/Research (ks5gh@virginia.edu)/1008 - Behavioral Design/b. Pattern Language/Data")

library(tidyverse)
library(textshape)
library(network)
library(reshape2)
library(ggplot2)
library(cowplot)
library(tidyverse)

# Import data

# Pattern Occurrences in Wider Set
df_occur <- read.csv('Occurences.csv')

# Behavioral Change Demand for case
df_demand <- read.csv('BehavioralSolutionMatrix.csv')

# Pattern relationships
df_comp <- read.csv('Complementary.csv')
df_inter_across <- read.csv('Interchangeable_AcrossPatterns.csv')
df_inter_within <- read.csv('Interchangeable_WithinCase.csv')
df_prop <- read.csv('Proportional.csv')


##################################################
# Graphing and measuring the distribution of patterns in the data collected

# Distribution of Pattern occurences
# DONE: restructure df for tidy framework
# Columns: {Pattern Name, Sum of occurences, Proportionality}


# Restructure df_occur to df_hist
df_hist <- df_occur %>% select(c(3:24)) %>% colSums() %>% as.data.frame()
df_hist$Pattern <- row.names(df_hist)   
colnames(df_hist)[colnames(df_hist) == "."] <- "Occurence"
df_hist$Pattern <- gsub(".", " ", df_hist$Pattern, fixed=TRUE)

# Use df_prop to merge pattern with level
df_hist <- as.data.frame(merge(x = df_hist, y = df_prop, by = c("Pattern", "Pattern")))

#df_hist$Scale <- as.factor(df_hist$Scale)
df_hist$Pattern <- as.factor(df_hist$Pattern)

# DONE: Plot in descending order with proportionality as the color
# Hypothesis: systems level patterns are underutilized
plot_hist <- ggplot(df_hist, aes(x= reorder(Pattern, - Occurence), y = Occurence, fill = Scale)) + 
  geom_bar(stat = "identity") + 
  labs(x= "Patterns", y = "Total Occurences")+
  theme_minimal_hgrid(12)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  theme(axis.text.x=element_text(size=6))+
  scale_fill_manual(values=c("#692E78", "#A074AB", "#BAAEDE"))+
  scale_y_continuous(expand = c(0,0)) 

plot_hist
# Reorder data structure for 3 bar graphs on different scales
# TODO: 3 bars for different scales v. total occurrence

df_bar_plot <- df_hist %>% select('Occurence', 'Scale')

df_bar_plot <- df_bar_plot %>%
  group_by(Scale) %>%
  summarise(Freq = sum(Occurence))

# Plot
plot_bargraph <- ggplot(df_bar_plot, aes(x= Scale, y = Freq, fill = Scale)) + 
  geom_bar(stat = "identity") + 
  labs(x= "Scale", y = "Total Occurences Of Patterns")+
  theme_minimal_hgrid(12)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_manual(values=c("#692E78", "#A074AB", "#BAAEDE"))+
  scale_y_continuous(expand = c(0,0)) 

plot_bargraph

# DONE: graph Number of patterns used in design v. Behavioral Change Demand
# Hypothesis: more patterns are needed for harder problems
# Refine problem solution matrix df
colnames(df_demand)

# Subset data
df_matrix <- df_demand %>% select('Design..', 'Case', 'Change.Demand...Novelty', 
                  'Change.Demand...Scope', 'Change.Demand...Frequency',
                  'Change.Demand..total.')
# Remove NAs
df_matrix <- df_matrix[rowSums(is.na(df_matrix)) == 0, ]

# use occurrences of patterns (df_occur) total count per case
colnames(df_occur)
which( colnames(df_occur)=="Simplifying.systems") #3
which( colnames(df_occur)=="Pings") #24

df_occur$TotalPattern <- rowSums(df_occur[3:24])
df_temp <- df_occur %>% select(Case, TotalPattern)


# Merging by inner join of Behavioral Problem Matrix info (df_matrix) and Occurences of patterns (df_temp)
df_temp <- df_temp %>% inner_join(df_matrix) 


# Linear Model
M1 <- lm(TotalPattern ~ Change.Demand..total., data = df_temp)
summary(M1)

# Plot Patterns Used In Design v. Behavioral Change Demand
ggplot(df_temp, aes(x= Change.Demand..total., y= TotalPattern)) + 
  geom_jitter(width = 0.2, height=0.2) +
  geom_smooth(method=lm) +
  labs(y= "Total Patterns Used for Case", x= "Behavioral Change Demand (Scope, Novelty, Frequency))")+
  theme_minimal_hgrid(12)

# Check residuals
E0 <- resid(M1)
F0 <- fitted(M1)

plot(x = F0 , y = E0, xlab = "Fitted values", ylab = "Residuals")

###################################################
# Network Analysis
# Purpose: Graph and measure the network of relationships between patterns
# Complementary and Interchangeable Relationships will be mapped

#########
# Network Preparation Function
# Works for within cases analysis
# Structure: dataframe with colnames = Case, Pattern 1, Pattern 2, ... Pattern N
# Values: Pattern 1 value == {0,1}


edge_list <- function(df) {
  # Code
  df <- df[, -which(names(df) == "Case")]
  # Convert to an adjacency matrix
  # For network analysis
  # D -> [m,n]
  my_mat <- as.matrix(df)
  # A = D^T X D
  A <- t(my_mat) %*% my_mat
  
  # Create edge list approach
  rows_cols_vals_matrices <-  list(row_indices = row(A), col_indices = col(A), values = A)
  vectorized_matrices <- lapply(rows_cols_vals_matrices, as.vector)
  edges <- do.call(cbind, vectorized_matrices)
  colnames(edges)<-c("from", "to", "weight")
  
  # Remove weight of 0 (no occurrences between cases)
  edges <- edges[edges[,3]>0,] 
  
  # in Tibble form
  edges <- as_tibble(edges)
}



################################################ 
# Graphing Networks

# import libraries
library(igraph)
library(tidygraph)
library(ggraph)
library(Rcpp)

# Create edges in tibble form
edges_comp <- edge_list(df_comp)
edges_int <- edge_list(df_inter_within)

# Create node list
# Node list = (ID, Label)
nodes_comp <- tibble(id = 1:ncol(df_comp),label = colnames(df_comp))
nodes_int <- tibble(id = 1:ncol(df_inter_within),label = colnames(df_inter_within))


# Tidy Graphs --------

# Create Tidy Graphs for each network
comp_tidy <- tbl_graph(nodes = nodes_comp, edges = edges_comp, directed = FALSE)
int_tidy <- tbl_graph(nodes = nodes_int, edges = edges_int, directed = FALSE)

# Ordered by highest weight
# comp_tidy %>% 
#   activate(edges) %>% 
#   arrange(desc(weight))

# Basic plot - complementary
ggraph(comp_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

# Weighted and labeled
# Complementary
graph_comp <- ggraph(comp_tidy, layout = "mds") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Co-occurence") +
  theme_graph()

graph_comp

# Interchangeable
graph_int <- ggraph(int_tidy, layout = "kk") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Co-occurence") +
  theme_graph()

graph_int


##################################
# Network Measures 
# TODO: community detection
# Particularly for complementary graphs

# Community detection 
# https://tidygraph.data-imaginist.com/reference/group_graph.html
# group_components()
degree(pattern_tidy)

# Node types: which nodes are central?
# node_is_keyplayer

# Which nodes are central
# node_connectivity_impact()

# Node rank


# Graph assortativity - why kinds of connections
# graph_assortativity()

# Resources for Network ----------
# Main Tutorial Used for Developing Script
# https://www.jessesadler.com/post/network-analysis-with-r/

# visualization
# http://kateto.net/network-visualization

# Analysis
# https://tidygraph.data-imaginist.com/reference/index.html#section-centrality
# http://sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r
