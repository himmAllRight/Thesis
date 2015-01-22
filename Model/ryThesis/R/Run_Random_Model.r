# Random Model Run. Randomly moves edges.
Run_Random_Model <- function(runCount, swpGraph, randGraph,  hubMatrix,
                             timeSteps){
  runLogOutput = paste('run',runCount,'_logOutput.txt', sep="")
  write(paste('step \t hubCount \t Sws \t avg_Path_Length \t Transitivity \t Clustering \t Sws2 '), 
        file= runLogOutput, append = TRUE, sep=",")
  print("in model Run now ") 
 
  degreeOutput = paste('run',runCount,'_DegreeLog.txt', sep="")

  degreeMax <- 0  # set window max for plotting degree distribution
  probMax   <- 0

  # Loops the model for specified amount of time (timeSteps)
  for ( step in seq(from=1, to=timeSteps, by=1)){
    print(paste("step: ",step))
    print(vcount(swpGraph))
    print(ecount(swpGraph))
    
    pickXY <- TRUE
    while(pickXY){
      x  <- sample(1:vcount(swpGraph), 1)  # X for swp Graph

      # makes list of all connected nodes to x and xR
      xNeighbors <- unlist(get.adjlist(swpGraph)[x])
      possibleY  <- xNeighbors[which(degree(swpGraph)[xNeighbors] > 1)]
#      print(possibleY)
 
      if(length(possibleY) < 1){
        # Rechoose x y
        pickXY <- TRUE
      }else if(length(possibleY) == 1){
        pickXY <- FALSE
        y <- possibleY[1]
      }else{
        pickXY <- FALSE
        y <- sample(possibleY, 1)
      } 
    }
    swpGraph[x,y]    <- FALSE              # Remove edge between x and y
    # Selects new z values that don't have an edge with x.
    z  <- sample(which(!(1:vcount(swpGraph) %in% xNeighbors)), 1)

    swpGraph[x,z] <- TRUE                  # Add edge between x and z



    # Checks to see if new degreeMax
    d  <- degree(swpGraph)
    dd <- degree.distribution(swpGraph)
    if(max(d) > degreeMax){
      degreeMax <- max(d)
    }
    if(max(dd) > probMax){
      probMax <- max(dd)
    }


    
    # Print attributes to output file
    # -------------------------------
    swsList <- CalcSws(swpGraph,randGraph)
    swpGamma  <-  transitivity(swpGraph, type="global", vids=NULL, weights=NULL)
    write(paste(step,'\t',HubCounts(FindHubs(runCount, hubThreshold, swpGraph)),
          '\t', swsList$Sws,'\t', swsList$swpPathLength,'\t',
          swsList$swpClustering,'\t', swsList$swpCC, '\t', swsList$Sws2 ),
          file= runLogOutput, append = TRUE, sep="," )


    # Print Degree Data
    PrintDegree(swpGraph, runCount, step)
    # Print Degree Distrribution Data
    PrintDegreeDist(swpGraph, runCount, step, timeSteps, degreeMax, probMax)

  }
}

