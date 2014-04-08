library(igraph)


p           <- .15
trialCounts <-  5
count       <-  1

# p Values loop
for( i in seq(from=1, to= 4, by= 1)){
  
  # for each trial loop
  for( trial in seq(from = 1, to= trialCounts, by=1)){
    g <- watts.strogatz.game(4,3,1,p)

    b <- betweenness(g)

    # Write to file
    betweennessOutput = paste("betweennessData_",count,".dat", sep="")

    for( v in seq(from=1, to=length(b), by= 1)){
      write(paste( v, '\t', b[v]), file = betweennessOutput, append = TRUE)

    }

    count = count + 1

  }

  p = p + .1

}
