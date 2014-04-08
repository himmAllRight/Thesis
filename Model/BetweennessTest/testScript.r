library(igraph)


trialCounts <-  5
count       <-  1


# Dim value loop
for( dim in seq(from=2, to=5, by=1)){

  # size values loop
  for( size in seq(from=2, to=7, by=1)){

    # p Values loop
    for( p in seq(from=.15, to= .45, by= .1)){
  
      # for each trial loop
      for( trial in seq(from = 1, to= trialCounts, by=1)){
        g <- watts.strogatz.game(4,size,1,p)

        b <- sort(betweenness(g))

        # Write to file
        betweennessOutput = paste("betweennessData_",count,".dat", sep="")

        for( v in seq(from=1, to=length(b), by= 1)){
          write(paste( v, '\t', b[v]), file = betweennessOutput, append = TRUE)
        }
        print(paste("Wrote data for sample ", count))
        count = count + 1

      }  

    } 
  }
}
