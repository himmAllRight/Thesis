## Ryan Himmelwright
## Honors Thesis
## Make Model Script
library(igraph)
library(base)

################################################################################
############################## Defined Functions ###############################
################################################################################
# Function to Generate Small World Graph
MakeSWPNetwork <- function(dim,size,nei,p){
  swpGraph <- watts.strogatz.game(dim,size,nei,p, loops = FALSE, multiple = FALSE)
  return(swpGraph)
}

# Function to Generate an Erdos-Renyi random graph
MakeRandNetwork <- function(dim, size, nei, swpGraph){
  # Try to make a random graph with the watts.strogatz.game function
  randGraph <- watts.strogatz.game(dim,size,nei,1,loops = FALSE, multiple = FALSE)
  return(randGraph)
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


################################################################################
##############################  Models Functions  ##############################
################################################################################

# Random Model Run. Randomly moves edges.
Run_Random_Model <- function(runCount, swpGraph, randGraph,  hubMatrix,
                             timeSteps){
  runLogOutput = paste('run',runCount,'_logOutput.txt', sep="")
  write(paste('step \t hubCount \t Sws \t avg_Path_Length \t Clustering'), 
        file= runLogOutput, append = TRUE, sep=",")
  
  # Loops the model for specified amount of time (timeSteps)
  for ( step in seq(from=1, to=timeSteps, by=1)){
    x<- sample(1:length(swpGraph), 1) # random int
    y<- sample(1:length(swpGraph), 1) # random int
    z<- sample(1:length(swpGraph), 1) # random int
    
    # Re-selects x and y if they don't have an edge between them.  
    while( swpGraph[x,y] == 0){
      x<- sample(1:length(swpGraph), 1)
      y<- sample(1:length(swpGraph), 1) 
    }
    swpGraph[x,y] <- FALSE              # Remove edge between x and y
    
    # Loops new z values until x and z don't have an edge
    while( swpGraph[x,z] == 1){
      z<- sample(1:length(swpGraph), 1)
          }
    swpGraph[x,z] <- 1                  # Add edge between x and z
    
#    print(swpGraph[])
    print(step)
    
    # Print attributes to output file
    # -------------------------------
    swsList = CalcSws(swpGraph,randGraph)
    swpGamma  =  transitivity(swpGraph, type="global", vids=NULL, weights=NULL)
    write(paste(step,'\t',HubCounts(FindHubs(runCount, hubThreshold, swpGraph)),
          '\t', swsList$Sws,'\t', swsList$swpPathLength,'\t',
          swsList$swpClustering), file= runLogOutput, append = TRUE, sep="," )
  }
 

}
Run_Hubs_Model <- function(swpGraph, randGraph, hubMatrix){

}

# Runs a model that only progresses forward if it increases pathlength.
Run_PathLength_Model <- function(swpGraph, randGraph, hubMatrix){

  runLogOutput = paste('run',runCount,'_logOutput.txt', sep="")
  write(paste('step \t hubCount \t Sws \t avg_Path_Length \t Clustering'), 
        file= runLogOutput, append = TRUE, sep=",")

  # Loops the model for specified amount of time (timeSteps)
  for ( step in seq(from=1, to=timeSteps, by=1)){
    print(step)
    old_PathLength = average.path.length(swpGraph)
    model_Drive = TRUE
    triedXList <- c()
    triedYList <- c()
    triedZList <- c()
    tryCount = 1
    while(model_Drive){
      vertCount = c(1:length(swpGraph))
      x <- sample(vertCount[which(!(vertCount%in%triedXList))], 1) # random int
      y <- sample(vertCount[which(!(vertCount%in%triedYList))], 1) # random int
      z <- sample(vertCount[which(!(vertCount%in%triedZList))], 1) # random int

#         Re-selects x and y if they don't have an edge between them.  
        while( swpGraph[x,y] == 0){
          x <- sample(1:length(swpGraph), 1)
          y <- sample(1:length(swpGraph), 1)
          }
        # Remove edge between x and y
        swpGraph[x,y] <- FALSE

        # Loops new z values until x and z don't have an edge
        while( swpGraph[x,z] == 1){
          z <- sample(1:length(swpGraph), 1)
          }
      
        # Add edge between x and z
        swpGraph[x,z] <- 1

      if(average.path.length(swpGraph) > old_PathLength){ # &&
#         !(x %in% triedList && y %in% triedList && z %in% triedList)){
        model_Drive = FALSE
        print(average.path.length(swpGraph))
        } else{
            triedXList <- append(triedXList, x)
            triedYList <- append(triedYList, y)
            triedZList <- append(triedZList, z)
            tryCount = tryCount + 1
        }
    }
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
PrintGraphStats <- function(runCount, swpGraph, randGraph, hubMatrix,dim, size,
                            nei, p, hubThreshold){
  # Generate output file of each run in each run directory
  outfileName = "../cumulative_attributes.txt"   
    
  for (i in seq(from=1, to=2, by=1)){
    write(paste('runCount: ', runCount), file= outfileName, append = TRUE, sep= ", ")
    write(paste('Dim: ', dim), file= outfileName, append = TRUE, sep= ", ")
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
    
    outfileName = paste('output_run',i,'.txt', sep="")
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
# Number of runs
trialCount= 3
timeSteps = 1000

## Model Parameters
dim           = 4   # Int Constant, the demension of the starting lattice
size          = 5   # The size of the lattice along each dimension
nei           = 1   # the neighborhood which the vert. of lattice will connect
p             = .3   # the rewiring probabillity
hubThreshold  = 0.8 # The threshold of the centrality score for determing a hub


# Generate Directories for all trials
runCount =1
for( i in seq(from=1, to= trialCount, by=1)){
  print(getwd()) # print current working directory
  
  print(paste('mkdir model_run',i, sep=""))
  system(paste('mkdir model_run',i, sep=""))
  setwd(paste('model_run',i, sep=""))
  print(getwd())

  #-----------------------------------------------
  #--------------- Model Sequence ----------------
  #-----------------------------------------------

  # Generate Graphs
  # ----------------
  notSWP = TRUE # true if the graphs are not swp
  while(notSWP){
    print("redo")
    swpGraph = MakeSWPNetwork(dim,size,nei,p)
    randGraph = MakeRandNetwork(dim, size, nei, swpGraph)
    if(CalcSws(swpGraph, randGraph)$Sws > 1) notSWP = FALSE
    }    
    
    # Run functions on Graphs
    # ------------------------
    hubMatrix = FindHubs(runCount, hubThreshold, swpGraph)
   # CalcSws = CalcSws(swpGraph, randGraph)
    PrintGraphStats(runCount, swpGraph, randGraph, hubMatrix, dim, size, nei, p,
                  hubThreshold)
#    plotGraph = PlotGraph(runCount, swpGraph, randGraph, hubMatrix)
#    rand_Model_Run = Run_Random_Model(runCount,swpGraph, randGraph, hubMatrix,
#                                     timeSteps)
 
    pathLength_Model_Run = Run_PathLength_Model(swpGraph, randGraph, hubMatrix)

    # Increment for next run
    # ----------------------
    runCount = runCount + 1
    setwd("..") # Go up a directory
}
print(warnings())
