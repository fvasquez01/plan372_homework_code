# Homework 5
# Load libraries
library(sf)
library(tidyverse)
library(tidygraph)
library(igraph)
library(units)

setwd("/Users/francevasquez/372projects/network_data")

# Load data
network <- st_read("network.gpkg")
points <- st_read("connected_points.gpkg")
unconnected_points <- st_read("unconnected_points.gpkg")

# Question 1 Base map ###
g1 <- ggplot() +
  geom_sf(data = network, aes(color = "Interconnections"), size = 0.5) +
  geom_sf(data = points, aes(color = "Connected Systems"), size = 2) +
  geom_sf(data = unconnected_points, aes(color = "Unconnected Systems"), size = 1, alpha = 0.5) +
  scale_color_manual(values = c("Connected Systems" = "blue", 
                                "Unconnected Systems" = "red",
                                "Interconnections" = "darkgreen"),
                     name = "Water Systems") +
  theme_minimal() +
  labs(title = "North Carolina Water System Interconnections")
g1

# Question 2 ###
edges <- network %>% mutate(edgeID = row_number())

coords <- st_coordinates(edges)
coords_tbl <- as_tibble(coords) %>%
  rename(edgeID = L1)

nodes <- coords_tbl %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(xy = paste(X, Y)) %>%
  distinct(xy, .keep_all = TRUE) %>%
  mutate(nodeID = row_number())

# Rebuild edges with from and to node IDs
coords_indexed <- coords_tbl %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(xy = paste(X, Y)) %>%
  left_join(nodes %>% select(xy, nodeID), by = "xy") %>%
  group_by(edgeID) %>%
  summarise(from = first(nodeID), to = last(nodeID))

edges <- edges %>%
  left_join(coords_indexed, by = "edgeID")

# Create node sf object
nodes_sf <- nodes %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(edges))

# Build graph
graph <- tbl_graph(nodes = nodes_sf, edges = edges, directed = FALSE) %>%
  activate(edges) %>%
  mutate(length = st_length(geom))

# Visualize base graph
q2_map <- ggplot() +
  geom_sf(data = st_as_sf(edges), color = "darkgreen", size = 0.5) +
  geom_sf(data = nodes_sf, color = "blue", size = 2) +
  theme_minimal() +
  labs(title = "Network Graph of Water System Interconnections")

#the graph visualized
q2_map

### Question 3 Centrality measures ###
graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length))

graph

graph %>% activate(nodes) %>% as_tibble() %>% View()

graph %>% activate(edges) %>% as_tibble() %>% View()


# Plot 1 Betweenness centrality at nodes
# Betweenness centrality at nodes
q3_node_plot <- ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'grey') + 
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0, 4)) +
  theme_minimal() +
  labs(title = "Betweenness Centrality (Nodes)")

### Plot 2 Betweenness centrality at edges ###
# Merge centrality back into edge sf from original edges
q3_edge_plot <- ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) +
  scale_colour_viridis_c(option = 'inferno') +
  scale_size_continuous(range = c(0, 4)) +
  theme_minimal() +
  labs(title = "Betweenness Centrality (Edges)")

q3_node_plot
q3_edge_plot

### Question 4 Shortest path from Cary to Chapel Hill ###

#Get the NodeIDs
node_labels <- nodes_sf %>%
  st_join(points, join = st_is_within_distance, dist = 10) %>%
  select(nodeID, Name) %>%
  distinct()

# Saved id's for the nodes
chapel_id <- 31    
cary_id <- 32      
raleigh_id <- 37   

# shortest paths
path_cary <- shortest_paths(
  graph = graph,
  from = cary_id,
  to = chapel_id,
  weights = graph %>% activate(edges) %>% pull(length),
  output = "both"
)

path_raleigh <- shortest_paths(
  graph = graph,
  from = raleigh_id,
  to = chapel_id,
  weights = graph %>% activate(edges) %>% pull(length),
  output = "both"
)

# Cary to Chapel Hill
cary_node_ids <- path_cary$vpath[[1]] %>% as.integer()

cary_route_names <- node_labels %>%
  filter(nodeID %in% cary_node_ids) %>%
  left_join(tibble(nodeID = cary_node_ids), by = "nodeID")  # keeps order by left join

print(cary_route_names)

# Raleigh to Chapel Hill
raleigh_node_ids <- path_raleigh$vpath[[1]] %>% as.integer()

raleigh_route_names <- node_labels %>%
  filter(nodeID %in% raleigh_node_ids) %>%
  left_join(tibble(nodeID = raleigh_node_ids), by = "nodeID")

print(raleigh_route_names)


# Create subgraphs
subgraph_cary <- graph %>% subgraph.edges(eids = path_cary$epath[[1]]) %>% as_tbl_graph()
subgraph_raleigh <- graph %>% subgraph.edges(eids = path_raleigh$epath[[1]]) %>% as_tbl_graph()

# Plots path from Cary to Chapel Hill
q4_cary_map <- ggplot() +
  geom_sf(data = st_as_sf(edges), color = "gray") +
  geom_sf(data = subgraph_cary %>% activate(edges) %>% as_tibble() %>% st_as_sf(), color = "blue", size = 1) +
  geom_sf(data = subgraph_cary %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), color = "black", size = 2) +
  theme_minimal() +
  labs(title = "Shortest Path from Cary to Chapel Hill")

# Plots path from Raleigh to Chapel Hill
q4_raleigh_map <- ggplot() +
  geom_sf(data = st_as_sf(edges), color = "gray") +
  geom_sf(data = subgraph_raleigh %>% activate(edges) %>% as_tibble() %>% st_as_sf(), color = "red", size = 1) +
  geom_sf(data = subgraph_raleigh %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), color = "black", size = 2) +
  theme_minimal() +
  labs(title = "Shortest Path from Raleigh to Chapel Hill")

#maps
q4_cary_map
q4_raleigh_map


### Question 5 Connect Liberty to nearest connected system ###

# Find Liberty by subsetting
liberty <- unconnected_points %>% filter(str_detect(tolower(Name), "liberty"))

# Calculate the nearest neighbor 
distances <- st_distance(liberty, points)
closest_system <- points[distances == min(distances), ][1, ]

# Extract coordinates
liberty_coords <- st_coordinates(liberty)[1, ]
closest_coords <- st_coordinates(closest_system)[1, ]

# Create data frame for plotting connection
connection_df <- tibble(
  X = c(liberty_coords[1], closest_coords[1]),
  Y = c(liberty_coords[2], closest_coords[2])
)

# Extracting the label positions
label_coords <- tibble(
  X = c(liberty_coords[1], closest_coords[1]),
  Y = c(liberty_coords[2], closest_coords[2]),
  label = c(as.character(liberty$Name), as.character(closest_system$Name))
)

# Plots map 
q5_map <- ggplot() +
  geom_sf(data = network, color = 'gray', size = 0.4) +
  geom_sf(data = points, color = 'blue', size = 2) +
  geom_sf(data = liberty, color = 'red', size = 3) +
  geom_sf(data = closest_system, color = 'green', size = 3) +
  geom_line(data = connection_df, aes(x = X, y = Y), color = 'purple', linewidth = 1) +
  geom_label(data = label_coords, aes(x = X, y = Y, label = label), size = 3, fill = "white") +
  theme_minimal() +
  labs(title = "Town of Liberty's Closest Option")
q5_map
