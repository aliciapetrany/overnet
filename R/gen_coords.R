
fruchterman_reingold <- function(graph, iterations = 100, temperature = sqrt(length(V(graph)))) {


  # Initialize positions randomly
  positions <- matrix(runif(2 * vcount(graph)), ncol = 2)

  for (iteration in 1:iterations) {
    # Calculate repulsive forces
    repulsive_forces <- matrix(0, nrow = vcount(graph), ncol = 2)
    for (i in 1:vcount(graph)) {
      for (j in 1:vcount(graph)) {
        if (i != j) {
          delta <- positions[i, ] - positions[j, ]
          repulsive_forces[i, ] <- repulsive_forces[i, ] + delta / sqrt(sum(delta^2))
        }
      }
    }

    # Calculate attractive forces
    attractive_forces <- matrix(0, nrow = vcount(graph), ncol = 2)
    for (edge in get.edgelist(graph)) {
      i <- edge[1]
      j <- edge[2]
      delta <- positions[i, ] - positions[j, ]
      attractive_forces[i, ] <- attractive_forces[i, ] - delta
      attractive_forces[j, ] <- attractive_forces[j, ] + delta
    }

    # Update positions
    displacement <- (attractive_forces + repulsive_forces) * temperature / sqrt(sum((attractive_forces + repulsive_forces)^2))
    positions <- positions + displacement

    # Cooling down the temperature
    temperature <- temperature * (1 - iteration/iterations)
  }

  return(positions)
}
