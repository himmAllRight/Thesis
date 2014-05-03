# Script to create a new graph generator that links several SWP graphs. This 
# code will then be implements into the models.

library(igraph)

#

generateGraph <- function(dim, size, nei, p, d){

  # Initial Swp Graph to make cumulative Swp
  G <- watts.strogatz.game(dim, size, nei, p)

  print("Initial G generation")
  print(paste("G nodes: ", vcount(G),"   G edges: ", ecount(G)))

  for (step in seq(from=1, to=(d - 1), by=1)){
    g <- watts.strogatz.game(dim, size, nei, p)

    G <- G + g

    print("G after addition")
    print(paste("G nodes: ", vcount(G),"   G edges: ", ecount(G)))
  }

  plot(G)

}


# input Parameters
args <- commandArgs(trailingOnly = TRUE)

# SWP parameters
dimension <- as.numeric(args[1])
size      <- as.numeric(args[2])
nei       <- as.numeric(args[3])
p         <- as.numeric(args[4])

# Group / Link parameters
d         <- 5


# Executable Code

run <- generateGraph(dimension, size, nei, p, d)
