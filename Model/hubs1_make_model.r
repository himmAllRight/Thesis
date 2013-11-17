## Ryan Himmelwright
## Honors Thesis
## Make Model Script
library(base)
library(igraph)
library(Matrix)
library(methods)
library(lattice)

################################################################################
############################## Defined Functions ###############################
################################################################################
# Function to Generate Small World Graph
MakeSWPNetwork <- function(dimension,size,nei,p){
  swpGraph <- watts.strogatz.game(dimension,size,nei,p, loops = FALSE, multiple = FALSE)
  return(swpGraph)
}

# Function to Generate an Erdos-Renyi random graph
MakeRandNetwork <- function(dimension, size, nei, swpGraph){
  # Try to make a random graph with the watts.strogatz.game function
  randGraph <- watts.strogatz.game(dimension,size,nei,1,loops = FALSE, multiple = FALSE)
  return(randGraph)
}

# Function to clean the graphs and remove the non connected nodes.
CleanGraphs <- function(swpGraph, randGraph){
  # Copies saved for printing comparisons  
  swp1  <- swpGraph
  rand1 <- randGraph

  # Clean Graphs
  # ------------
  swpDeadNodes = (which(degree(swpGraph) < 1))
  randDeadNodes= (which(degree(randGraph) < 1))

  print(length(swpDeadNodes))
  print(length(randDeadNodes))

  # Removes unconnected nodes from generated graphs.
  print("pre initial clean")
  swpGraph  <- delete.vertices(swpGraph, swpDeadNodes)
  randGraph <- delete.vertices(randGraph, randDeadNodes)
  print("post initial clean")

  deadNodeLengths  = c(length(swpDeadNodes), length(randDeadNodes))
  extraDead <- (deadNodeLengths[1] - deadNodeLengths[2])

  if( extraDead > 0 ){
    print("if 1")
  # remove abs() random nodes from rand
    startE <- ecount(randGraph)
    randGraph <- delete.vertices(randGraph,sample(1:vcount(randGraph),
                                 abs(extraDead)))
    endE <- ecount(randGraph)

    # Adds edges until the starting number is reached.
    while( ecount(randGraph) < startE){
      randGraph <- add.edges(randGraph, sample(1:vcount(randGraph),2),
                             multiple = FALSE, loops = FALSE)
    }

  } else if( extraDead < 0){
    print("if 2")
    # remove abs() random nodes from swp
    startE <- ecount(swpGraph)
    swpGraph <- delete.vertices(swpGraph,sample(1:vcount(swpGraph),
                                abs(extraDead)))
    endE <- ecount(swpGraph)

    # Adds edges until the starting number is reached.
    while( ecount(swpGraph) < startE){
      swpGraph <- add.edges(swpGraph, sample(1:vcount(swpGraph),2),
                            multiple = FALSE, loops = FALSE )
      }
    }

  # Array to return with all the pre and post cleaned graphs.
  graphList <- list("swp1" = swp1, "rand1" = rand1, "swpGraph" = swpGraph, 
                    "randGraph" = randGraph)

  return(graphList)
  
}

# Finds the hubs of a network.
FindHubs <- function(runCount, hubThreshold, swpGraph){
  hubScore  = hub.score(swpGraph) 
  hubValues = hubScore$vector      # Takes just values from hub score
  # Replaces all hubs with a 1, and other vertices with a 0.
  hubMatrix = replace(replace(hubValues,hubValues >= hubThreshold, 1),
                              hubValues < hubThreshold,0)  
  return(hubMatrix)
}

# Returns the number of hubs in the Matrix
HubCounts <- function(hubMatrix){
  count = sum(hubMatrix == 1)
  return(count)
}


# Calculates S^WS for the network.
CalcSws <- function(swpGraph, randGraph){
  # Calculates clustering coefficients of swp and rand graphs
  swpGamma  =  transitivity(swpGraph, type="global", vids=NULL, weights=NULL,
                            isolates="zero")								
  randGamma = transitivity(randGraph, type="global", vids=NULL, weights=NULL,
                           isolates="zero")                         
  gamma = (swpGamma/randGamma) # combines them to get the Gamma value.

  # Calculates the mean minmal path length for swp and corresponding rand graphs
  swpLambda = average.path.length(swpGraph)
  randLambda = average.path.length(randGraph)
  lambda = (swpLambda / randLambda)  # Combines to get the ratio Lambda value

  Sws = (gamma/lambda) # Calculates S^WS from the ratio.
  
  swsList <- list("Sws" = Sws, "swpPathLength" = swpLambda, 
                  "swpClustering" = swpGamma)
  return(swsList)
}

# Returns two lists containing all the hub-hub connections.
HubHub <- function(swpGraph, hubs){
  hubhub1 <- c()
  hubhub2 <- c()

  for(hubN in hubs){
    adjHubs <- intersect(unlist(get.adjlist(swpGraph)[hubN]), hubs)
    if(length(adjHubs) > 0){
      for( m in seq(from=1, to=length(adjHubs), by=1)){
        hubhub1 <- c(hubhub1, hubN)
        hubhub2 <- c(hubhub2, adjHubs[m])
      }
    }
  }

  hubhub <- list("hubhub1" = hubhub1, "hubhub2"=hubhub2)
  return(hubhub)
}

################################################################################
##############################  Models Functions  ##############################
################################################################################

# Run model that attacks the hubs first.
Run_Hubs_Model <- function(runCount, swpGraph, randGraph, hubThreshold, 
                           timeSteps){
  # Model Print Out 
  runLogOutput = paste('run',runCount,'_logOutput.txt', sep="")
  write(paste('step \t hubCount \t Sws \t avg_Path_Length \t Clustering'), 
        file= runLogOutput, append = TRUE, sep=",")

 # Returns a list of the vertex number of all the hubs. 
  for(step in seq(from=1, to=timeSteps, by=1)){
    # Swp Hubs
    swpHubMatrix  = FindHubs(runCount, hubThreshold, swpGraph)
    swpHubInd     = (which(swpHubMatrix %in% 1))
    swpNonHubs    <- which(!(1:length(swpHubMatrix) %in% swpHubInd))
    # Rand Hubs
    randHubMatrix = FindHubs(runCount, hubThreshold, randGraph)
    randHubInd    = (which(randHubMatrix %in% 1))
    randNonHubs   <- which(!(1:length(randHubMatrix) %in% randHubInd))
    
    # SWP hub-hub connections
    hubhub  <- HubHub(swpGraph, swpHubInd)
    hubhub1 <- hubhub$hubhub1  # used for x
    hubhub2 <- hubhub$hubhub2  # used for y
   
    # SWP hub-hub connections
    randHubHub  <- HubHub(randGraph, randHubInd)
    randHubHub1 <- HubHub(randGraph, randHubInd)
    randHubHub2 <- HubHub(randGraph, randHubInd)

    # SWP Alg: If there are no Hubs
    if(length(swpHubInd) < 1){
     print("swp if 1")
      #terminate 

    }else{
      # If there are hub-hub connections
      if(length(hubhub1) >= 2){

print("swp else if")
        # Takes a sample x from the list of connected hubs.
        xInd <- sample(1:length(hubhub1), 1) # Ind of a hub-hub connection
        x    <- hubhub1[xInd]                # node x for that connection
        y    <- hubhub2[xInd]                # node y for that connection
 
        swpGraph[x,y]    <- FALSE   # Removes edge between x and y
    
        nonAdjZ <- which(!(1:vcount(swpGraph) %in% unlist(get.adjlist(swpGraph)[x])))
        z  <- sample( intersect(swpNonHubs,nonAdjZ) , 1 ) 
        swpGraph[x,z] <- 1             # Adds edge between x and z
    
      }
      # If there are hubs, but no hub-hub connections
      else if( length(hubhub1) < 2 ){

print("swp else else if")

        x <- sample(swpHubInd, 1)   # Random hub node
        y <- sample(unlist(get.adjlist(swpGraph)[x]) , 1) # Random x-adj, non-hub    
        swpGraph[x,y] <- FALSE #Removes edge between x and y
        nonAdjZ <- which(!(1:vcount(swpGraph) %in% unlist(get.adjlist(swpGraph)[x])))
        z  <- sample( intersect(swpNonHubs,nonAdjZ) , 1 ) 
        swpGraph[x,z] <- 1            # Adds edge between x and z
      }
}
    


    # Rand: If there are no Hubs
    if(length(randHubInd) < 1){
print("rand if")
      #terminate 

    }else{
      # If there are hub-hub connections
      if(length(randHubHub1) >= 2){
print("rand else if")
        # Takes a sample x from the list of connected hubs.
        xRInd <- sample(1:length(randHubHub1), 1) # Ind of a hub-hub connection
        xR    <- randHubHub1[xRInd]                # node x for that connection
        yR    <- randHubHub2[xRInd]                # node y for that connection

        randGraph[xR,yR]    <- FALSE   # Removes edge between x and y

        nonAdjZR <- which(!(1:vcount(swpGraph) %in% unlist(get.adjlist(swpGraph)[x])))
        zR  <- sample( intersect(swpNonHubs,nonAdjZ) , 1 ) 
        randGraph[xR,zR] <- 1             # Adds edge between x and z

      }
      # If there are hubs, but no hub-hub connections
      else if( length(randHubHub1) < 2 ){
print("rand else else if")
        xR <- sample(randHubInd, 1)   # Random hub node
        yR <- sample(unlist(get.adjlist(randGraph)[xR]) , 1) # Random x-adj, non-hub

        randGraph[xR,yR] <- FALSE #Removes edge between x and y

        nonAdjZR <- which(!(1:vcount(swpGraph) %in% unlist(get.adjlist(swpGraph)[x])))
        zR  <- sample( intersect(swpNonHubs,nonAdjZ) , 1 ) 
        randGraph[xR,zR] <- 1            # Adds edge between x and z
      }
    }


    print(paste('step: ', step))
      
    # Print attributes to output file
    # -------------------------------
    swsList = CalcSws(swpGraph,randGraph)
    swpGamma  =  transitivity(swpGraph, type="global", vids=NULL, weights=NULL)
    write(paste(step,'\t',HubCounts(FindHubs(runCount, hubThreshold, swpGraph)),
          '\t', swsList$Sws,'\t', swsList$swpPathLength,'\t',
          swsList$swpClustering), file= runLogOutput, append = TRUE, sep="," )
    }
}
################################################################################
############################## Printing Functions ##############################
################################################################################
PrintGraphStats <- function(runCount, swpGraph, randGraph, hubMatrix,dimension, size,
                            nei, p, hubThreshold){
  # Generate output file of each run in each run directory
  outfileName = "../cumulative_attributes.txt"   
    
  for (i in seq(from=1, to=2, by=1)){
    write(paste('runCount: ', runCount), file= outfileName, append = TRUE, sep= ", ")
    write(paste('dimension: ', dimension), file= outfileName, append = TRUE, sep= ", ")
    write(paste('Size: ',size), file= outfileName, append = TRUE, sep= ", ")
    write(paste('Nei: ', nei), file= outfileName, append = TRUE, sep= ", ")
    write(paste('p: ',p), file= outfileName, append = TRUE, sep= ", ")
    write(paste('hubThreshold: ', hubThreshold), file= outfileName,
          append = TRUE, sep= ", ")
    write(paste('swpGraph Vertice count: ', vcount(swpGraph)), file= outfileName,
          append = TRUE, sep= ", ")
    write(paste('swpGraph Edge count: ',ecount(swpGraph)), file= outfileName, 
          append = TRUE, sep= ", ")
    write(paste('swpGraph Sws: ', CalcSws(swpGraph, randGraph)$Sws), file= outfileName, append = TRUE, 
          sep= ", ")
    write(paste('swpGraph Hub count: ', sum(hubMatrix == 1)), file= outfileName,
          append = TRUE, sep= ", ")
    write('', file= outfileName, append = TRUE)
    
    outfileName = paste('starting_params.txt', sep="")
    }
}

PlotGraph <- function(runCount, swpGraph, randGraph, hubMatrix){
  # Color hubs in SWP plot.
  for (i in seq(from=1, to= length(hubMatrix), by=1)){
    if(hubMatrix[i] == 1){
      V(swpGraph)$color[i] = "green"
      }
      else{
        V(swpGraph)$color[i] = "cyan"
      }
  }
  ## Plot
  png(file=(paste("SWPplot",runCount,".png",sep="")))
  plot(swpGraph)
  png(file="rand_plot1.png")
  plot(randGraph)
}

################################################################################
################################ Execution Code ################################
################################################################################
args <- commandArgs(trailingOnly = TRUE)
topFolder <- args[1]
name 	  <- args[2]
dimension <- as.numeric(args[3])
size      <- as.numeric(args[4])
nei  	  <- as.numeric(args[5])
p    	  <- as.numeric(args[6])

# Number of runs
trialCount= as.numeric(args[7])
timeSteps = as.numeric(args[8])

setwd(topFolder)


hubThreshold  = 0.8 # The threshold of the centrality score for determing a hub

# Generate Directories for all trials
runCount =1

for( i in seq(from=1, to= trialCount, by=1)){

  print(getwd()) # print current working directory
  
  print(paste('mkdir ',name, sep=""))
  system(paste('mkdir ', name, sep=""))
  setwd(paste(name, sep=""))
  print(getwd())

  #-----------------------------------------------
  #--------------- Model Sequence ----------------
  #-----------------------------------------------

  # Generate Graphs
  # ----------------
  notSWP      = TRUE # true if the graphs are not swp
  notSWPCount = 0
  while(notSWP){
    print("redo")
    notSWPCount = notSWPCount + 1
    print(notSWPCount)
    swpGraph = MakeSWPNetwork(dimension,size,nei,p)
    randGraph = MakeRandNetwork(dimension, size, nei, swpGraph)
    if(CalcSws(swpGraph, randGraph)$Sws > 1) notSWP=FALSE
    if(notSWPCount >= 1000){
	    write(paste('Could not generate SWP graph in ', notSWPCount, 
	  	            ' tries.'), file= 'failed.txt', append = TRUE, sep= ", ")
	    quit(save = "no")
      }
    }    
    
  # Clean Graphs
  graphList <- CleanGraphs(swpGraph, randGraph)
  swp1        <- graphList$swp1      # Origonal swp for comparison
  rand1       <- graphList$rand1     # Origonal rand for comparison
  swpGraph    <- graphList$swpGraph  # Cleaned swp Graph
  randGraph   <- graphList$randGraph # Cleaned rand Graph



    # Run functions on Graphs
    # ------------------------
    hubMatrix = FindHubs(runCount, hubThreshold, swpGraph)
   # CalcSws = CalcSws(swpGraph, randGraph)
    PrintGraphStats(runCount, swpGraph, randGraph, hubMatrix, dimension, size, nei, p,
                    hubThreshold)

    hubs_Model_run = Run_Hubs_Model(runCount, swpGraph, randGraph, hubThreshold, 
                           timeSteps)

    # Increment for next run
    # ----------------------
    runCount = runCount + 1
    setwd("..") # Go up a directory
}
print(warnings())
