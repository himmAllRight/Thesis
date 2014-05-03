# Script to create a new graph generator that links several SWP graphs. This 
# code will then be implements into the models.

library(igraph)

#

generateGraph <- function(dim, size, nei, p, n, d){

  # Initial Swp Graph to make cumulative Swp
  G <- watts.strogatz.game(dim, size, nei, p)

  print("Initial G generation")
  print(paste("G nodes: ", vcount(G),"   G edges: ", ecount(G)))

  for (i in seq(from=1, to=(n - 1), by=1)){
    g <- watts.strogatz.game(dim, size, nei, p)

    G <- G + g

    print("G after addition")
    print(paste("G nodes: ", vcount(G),"   G edges: ", ecount(G)))
  }

  # connect components
  gL <- vcount(g)

  # Connect all subgraphs to each other
  # each subgraph
  for(i in seq(from=1, to= n, by=1)){
    # Link to each proceding sub-graph
    k =

    for( j in seq(from=i+1, to= n, by=1)){
      

      x <- sample( (((i-1)*gL)+1):(i*gL) ,1)
      y <- sample( (((j-1)*gL)+1):(j*gL) ,1)

      print(x)
      print(y)

      G[x,y] <- TRUE
    }

  }


  # Connect each subgraph to first subgraph
  # for(i in seq(from=1, to=(n-1), by=1)){
  #   # for each d
  #   for(j in seq(from=1, to=d, by=1)){
  #     x <- sample(1:gL,1)
  #     y <- sample(((gL*i)+1):(gL*(i+1)),1)

  #     G[x,y] <- TRUE
  #   }

  # }


  return(G)
}


# input Parameters
args <- commandArgs(trailingOnly = TRUE)

# SWP parameters
dimension <- as.numeric(args[1])
size      <- as.numeric(args[2])
nei       <- as.numeric(args[3])
p         <- as.numeric(args[4])

# Group / Link parameters
n         <- 5
d         <- 100


# Executable Code

run <- generateGraph(dimension, size, nei, p, n, d)


#plot Graph
png(file="testGraphGen.png")
plot(run)