# Load required libraries
# install.packages("datastructures", repos = "http://cran.us.r-project.org")
# Load required libraries
library(igraph)

# 1
N <- 9
G_size <- sample(N+10:((N/10+5)^2+5*N), 1)
g <- make_ring(G_size)
cat("Number of vertices:", vcount(g), "\n")
cat("Number of edges:", ecount(g), "\n")
adj_matrix <- as_adjacency_matrix(g)
plot(g)
print(adj_matrix)

# 2
g1 <- make_empty_graph(G_size)
V(g1)$color <- "yellow"
E(g1)$color <- NA

random_edges <- sample(1:(G_size-1), N*8, replace = TRUE)
edges_to_add <- matrix(random_edges, ncol = 2, byrow = TRUE)
g1 <- g1 + edges(edges_to_add, color = "red")
plot(g1)
print(as_adjacency_matrix(g1))

random_edges <- sample(1:(G_size-1), N*10, replace = TRUE)
edges_to_add <- matrix(random_edges, ncol = 2, byrow = TRUE)
g1 <- g1 + edges(edges_to_add, color = "blue")
plot(g1)
print(as_adjacency_matrix(g1))

# 3
custom_edges <- c(2*N+23, 2*N+20, 2*N+12, N+15, 2*N-1, N+8, 2*N, 2*N+1, N+7, N+13)
edges_to_add <- matrix(custom_edges, ncol = 2, byrow = TRUE)

for (i in seq(1, nrow(edges_to_add), by = 1)) {
  if (all(edges_to_add[i,] %in% V(g1))) {
    g1 <- g1 + edges(edges_to_add[i,], color = "black")
  }
}

plot(g1)
cat("Neighbors of the", N, "th vertex:\n")
print(neighbors(g1, N))
cat("Edges incident to the", N, "th vertex:\n")
print(incident(g1, N))

cat("Are vertices", N+10, "and", N+12, "connected?\n")
cat(are_adjacent(g1, N+10, N+12), "\n")
print(as_adjacency_matrix(g1))

# 4
# Create a vector of uppercase letters
uppercase_letters <- LETTERS

# Generate names for vertices
vertex_names <- NULL

# Calculate the number of iterations needed
num_iterations <- ceiling((G_size + 1) / length(uppercase_letters))

# Generate names for each iteration
for (i in 1:num_iterations) {
  start_index <- (i - 1) * length(uppercase_letters) + 1
  end_index <- min(i * length(uppercase_letters), G_size + 1)

  current_names <- c(uppercase_letters, paste0(uppercase_letters, uppercase_letters))

  # Add names to the vertex_names vector
  vertex_names <- c(vertex_names, current_names[start_index:end_index])
}

vertex_to_add <- which.max(degree(g1))
g1 <- add_vertices(g1, 1)
g1 <- add_edges(g1, c(vertex_to_add, vcount(g1)))
V(g1)$name <- vertex_names[1:(G_size+1)]
print(as_adjacency_matrix(g1))

selected_vertices <- V(g1)[degree(g1) < 5 & degree(g1) > 2]
cat("Vertices with degree less than 5 and greater than 2:\n")
print(selected_vertices)

# 5
layouts <- list(
  in_circle = layout_in_circle(g1),
  in_tree = layout_as_tree(g1),
  lattice = layout_on_grid(g1)
)

# You can try the layouts using plot function, e.g.:
plot(g1, layout = layouts$in_circle)
plot(g1, layout = layouts$in_tree)
plot(g1, layout = layouts$lattice)


cat("Are vertices", N+10, "and", N+12, "connected?\n")
if (all(c(N+10, N+12) %in% V(g1))) {
  cat(are_adjacent(g1, N+10, N+12), "\n")
} else {
  cat("Invalid vertex IDs\n")
}
print(as_adjacency_matrix(g1))

# 6
cat("Diameter of the graph g1:", diameter(g1), "\n")
all_shortest_paths <- shortest_paths(g1, from = V(g1))
cat("Shortest paths for each vertex:\n")
print(all_shortest_paths$vpath)
V(g1)$label.cex <- 0.5 + degree(g1)/max(degree(g1))
plot(g1)


# TASK 2

# # Example usage
# N <- 5
# K <- 2
# roads <- list(
#   c(1, 2, 2),
#   c(2, 3, 4),
#   c(1, 4, 6),
#   c(4, 5, 3)
# )

# Dijkstra's algorithm function
dijkstra <- function(graph, source) {
  n <- nrow(graph)
  dist <- rep(1e9, n)
  dist[source] <- 0
  visited <- rep(FALSE, n)

  for (i in 1:n) {
    u <- -1

    for (j in 1:n) {
      if (!visited[j] && (u == -1 || dist[j] < dist[u])) {
        u <- j
      }
    }

    visited[u] <- TRUE

    for (v in 1:n) {
      if (!visited[v] && graph[u, v] < 1e9) {
        new_dist <- dist[u] + graph[u, v]
        if (new_dist < dist[v]) {
          dist[v] <- new_dist
        }
      }
    }
  }

  return(dist)
}

# Read input
# N <- as.integer(readline(prompt = "Enter the number of houses (N): "))
# K <- as.integer(readline(prompt = "Enter the number of roads (K): ")) <-

N <- 6 # Number of houses
K <- 7 # Number of roads

# Initialize distance matrix with large values
graph <- matrix(1e9, nrow = N, ncol = N)

# Roads: (A, B, length)
roads <- list(
  c(1, 2, 4),
  c(1, 3, 2),
  c(2, 3, 1),
  c(2, 4, 5),
  c(3, 4, 8),
  c(3, 5, 10),
  c(4, 6, 6)
)

# Read roads and update graph
for (i in 1:K) {
  # road <- as.integer(unlist(strsplit(readline(prompt = "Enter road details (A, B, length): "), " ")))
  road <- roads[[i]]
  graph[road[1], road[2]] <- road[3]
  graph[road[2], road[1]] <- road[3]
}

# Calculate all-pairs shortest paths using Dijkstra's algorithm
all_pair_shortest_paths <- lapply(1:N, function(source) dijkstra(graph, source))
dist_matrix <- do.call(rbind, all_pair_shortest_paths)

# Find the house with minimum sum of distances
min_distance <- sum(dist_matrix[1, ])
meeting_point <- 1

for (i in 2:N) {
  curr_distance <- sum(dist_matrix[i, ])
  if (curr_distance < min_distance) {
    min_distance <- curr_distance
    meeting_point <- i
  }
}

cat("The optimal meeting point is house number", meeting_point, "\n")
