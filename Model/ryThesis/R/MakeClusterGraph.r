MakeClusterGraph <- function(dim, size, nei, p, n, d){

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
  for(i in seq(from=1, to= n-1, by=1)){
    # Link to each proceding sub-graph
    

      # For each other subgraph connection
      for(j in seq(from=(i + 1), to= n, by=1)){
        for(k in seq(from= 1, to= d, by=1)){
          x <- sample( (((i-1)*gL)+1):(i*gL) ,1)
          y <- sample( (((j-1)*gL)+1):(j*gL) ,1)

          print(x)
          print(y)

          G[x,y] <- TRUE
        }
      }

  }
