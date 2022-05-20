# To analyze relationships of patterns
# Network analysis with key assumptions:
# Unidirectional ( symmetrical matrix), No loops, therefore zero on diagonals, and accounts for multiple edges


# Preprocessing
# Clearing workspace
rm(list = ls())

# Set Working directory

library(tidyverse)
library(textshape)
library(network)
library(reshape2)

# Set working directory
setwd("~/Library/CloudStorage/Box-Box/Research (ks5gh@virginia.edu)/1008 - Behavioral Design/b. Pattern Language/Data")

# Import data

# Pattern Frequency

# Pattern relationships
df_compl <- read.csv('Pattern relationships - Complementary.csv')
df_inter_across <- read.csv('Pattern relationships - Interchangeable_AcrossPatterns.csv')
df_inter_within <- read.csv('Pattern relationships - Interchangeable_WithinCase.csv')
df_prop <- read.csv('Pattern relationships - Proportional_AcrossPatterns.csv')

# Analysis of 


###################################################
# Network Analysis


# Convert to an adjacency matrix
# For network analysis
df <- df_import[, -which(names(df_import) == "Case")]

# D -> [m,n]
my_mat <- as.matrix(df)
# A = D^T X D
A <- t(my_mat) %*% my_mat


# TODO: Might have to clear out the diagonal on the adjacency matrix or not graph it tbd

# Preliminary plot
adj_net <- graph_from_adjacency_matrix(A, mode = "undirected") 
plot(adj_net)


# Alternative edge list approach
rows_cols_vals_matrices <-  list(row_indices = row(A),
                                 col_indices = col(A), 
                                 values = A)
rows_cols_vals_matrices
vectorized_matrices <- lapply(rows_cols_vals_matrices, as.vector)
vectorized_matrices
edges <- do.call(cbind, vectorized_matrices)
head(edges)
colnames(edges)<-c("from", "to", "weight")


# Network Package function ; Alternative creation of edge list
# net<-as.network(A,matrix.type='adjacency',
#                 ignore.eval = FALSE,  # read edge values from matrix as attribute
#                 names.eval= 'pattern', # name the attribute
#                 loops=FALSE)   # ignore self-edges
# 
# # Edge List
# edges <-as.edgelist(net, attrname = 'pattern', directed = FALSE)
# is.edgelist(edges)
# # rename Edge List
# colnames(edges)<-c("from", "to", "weight")

# Node list = (ID, Label)
nodes <- tibble(id = 1:ncol(df),label = colnames(df))

# Pattern Network
pattern_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", names.eval = "label", 
                           ignore.eval = FALSE, directed = FALSE, loops = FALSE)

# Check the Structure of the Network
class(pattern_network)

# Preliminary Plot
plot(pattern_network, vertex.cex = 3, mode = "circle")


# Clean up R environment for Graphing and Analysis--------------------
detach(package:network)
rm(pattern_network)
library(igraph)


# Graph
pattern_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
pattern_igraph
# ^ should return: undirected network (U) that has a name attribute (N) and is weighted (W)
# nodes and edges are the numbers following

plot(pattern_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)
  
# More Graphing Libraries
library(tidygraph)
library(ggraph)


edges <- as_tibble(edges)
pattern_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)


# Alternative
pattern_igraph_tidy <- as_tbl_graph(pattern_igraph)

# Ordered by highest weight
pattern_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

# Plots -----------

# Basic plot
ggraph(pattern_tidy) + geom_edge_link() + geom_node_point() + theme_graph()


library(Rcpp)


# Weighted and labeled
ggraph(pattern_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Co-occurence") +
  theme_graph()

# Interactive Plots -------------
library(visNetwork)
library(networkD3)


# 2D Plots
edges <- as.data.frame(edges)
visNetwork(nodes, edges)

# Adjust Weights on Edges
edges <- mutate(edges, width = weight/5 + 1)

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges()


# 3D plots
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)


# Network analysis ------------------------------
# Probably use tidygraph package

# Centrality - what measures are best
pattern_tidy%>%
  mutate(centrality = centrality_authority()) %>%
  ggraph(layout = 'kk') + 
  geom_node_point(aes(size = centrality, color = centrality)) +
  geom_edge_link() +
  scale_color_continuous(guide = 'legend') +
  theme_graph()



# Community detection 
# https://tidygraph.data-imaginist.com/reference/group_graph.html
# group_components()

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
