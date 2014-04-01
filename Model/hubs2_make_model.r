## Ryan Himmelwright
## Honors Thesis
## Make Model Script
library(base)
library(igraph)
library(Matrix)
library(methods)
library(lattice)

# Generate Random Seed Value
seedValue <- sample(1:50000,1)
set.seed(seedValue)

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
  #randGraph <- watts.strogatz.game(dimension,size,nei,1,loops = FALSE, multiple = FALSE)
  randGraph <- erdos.renyi.game(vcount(swpGraph),ecount(swpGraph), type="gnm",
                               directed = FALSE, loops = FALSE)
  return(randGraph)
}


# Finds the hubs of a network.
FindHubs <- function(runCount, hubThreshold, swpGraph){

# Part to replace in hubs2s

  #hubScore  <- hub.score(swpGraph) 
  #hubValues <- hubScore$vector      # Takes just values from hub score

  betweenValues <- betweenness(swpGraph)
  

# Might not be needed with new hubs
  # Replaces all hubs with a 1, and other vertices with a 0.
  hubMatrix <- replace(replace(hubValues,hubValues >= hubThreshold, 1),
                              hubValues < hubThreshold,0)  
  return(hubMatrix)
}

# Returns the number of hubs in the Matrix
HubCounts <- function(hubMatrix){
  count <- sum(hubMatrix == 1)
  return(count)
}


# Calculates S^WS for the network.
CalcSws <- function(swpGraph, randGraph){
  # Calculates clustering coefficients of swp and rand graphs
  # old
  swpGamma  <-  transitivity(swpGraph, type="global", vids=NULL, weights=NULL,
                            isolates="Nan")
  # New Clustering Coefficient Calc
  swpCC     <- transitivity(swpGraph, type='localaverageundirecte', vids=NULL,
                            weights=NULL, isolates="NaN")
  
  # old								
  randGamma <- transitivity(randGraph, type="global", vids=NULL, weights=NULL,
                            isolates="Nan")                         

  # New Clustering Coefficient Calc
  randCC    <- transitivity(randGraph, type='localaverageundirected', vids=NULL,
                            weights=NULL, isolates="NaN")

  gamma     <- (swpGamma/randGamma) # combines them to get the Gamma value.
  gamma2    <- (swpCC/randCC) # Gamma w/ new CC calcs

  # Calculates the mean minmal path length for swp and corresponding rand graphs
  swpLambda <- average.path.length(swpGraph)
  randLambda <- average.path.length(randGraph)
  lambda <- (swpLambda / randLambda)  # Combines to get the ratio Lambda value

  Sws       <- (gamma/lambda) # Calculates S^WS from the ratio.
  Sws2      <- (gamma2/lambda) # Sws with new CC calculation
  
  swsList <- list("Sws" = Sws, "swpPathLength" = swpLambda, 
                  "swpClustering" = swpGamma, "swpCC" = swpCC, "Sws2"=Sws2)
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
  
  # Initialzie Model Print Out files
  # Graph Attributes
  runLogOutput = paste('run',runCount,'_logOutput.txt', sep="")
  write(paste('step \t hubCount \t Sws \t avg_Path_Length \t Transitivity \t Clustering \t Sws2 '), 
        file= runLogOutput, append = TRUE, sep=",")

  degreeOutput = paste('run',runCount,'_DegreeLog.txt', sep="")

 # Returns a list of the vertex number of all the hubs. 
  for(step in seq(from=1, to=timeSteps, by=1)){
    # Swp Hubs
    swpHubMatrix  <- FindHubs(runCount, hubThreshold, swpGraph)
    swpHubInd     <- (which(swpHubMatrix %in% 1))
    swpNonHubs    <- which(!(1:length(swpHubMatrix) %in% swpHubInd))

print(vcount(swpGraph))
print(ecount(swpGraph))
    
    # SWP hub-hub connections
    hubhub  <- HubHub(swpGraph, swpHubInd)
    hubhub1 <- hubhub$hubhub1  # used for x
    hubhub2 <- hubhub$hubhub2  # used for y
   
print(paste('step: ', step))
    if(length(swpHubInd) < 1){
      print("terminate")
      #terminate 

    }else{
      # If there are hub-hub connections
      if(length(hubhub1) >= 2){
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
        # If there is only 1 hub.
        if( length(swpHubInd) == 1){
          print("hub1")
          x <- swpHubInd[1]       # Select the hub
          y <- sample(unlist(get.adjlist(swpGraph)[x]), 1) # Random x-adj
          swpGraph[x,y] <- FALSE  # Removes edge between x and y
          nonAdjZ <- which(!(1:vcount(swpGraph) %in% unlist(get.adjlist(swpGraph)[x])))
          z <- sample( intersect(swpNonHubs,nonAdjZ), 1)
          swpGraph[x,z] <-1
        }else{ # If there is more than 1 hub
          x <- sample(swpHubInd, 1)   # Random hub node
          y <- sample(unlist(get.adjlist(swpGraph)[x]) , 1) # Random x-adj, non-hub    
          
          swpGraph[x,y] <- FALSE #Removes edge between x and y
          nonAdjZ <- which(!(1:vcount(swpGraph) %in% unlist(get.adjlist(swpGraph)[x])))
          z  <- sample( intersect(swpNonHubs,nonAdjZ) , 1 ) 
          swpGraph[x,z] <- 1            # Adds edge between x and z
        }
      }
    }
    


    # Print attributes to output file
    # -------------------------------
    swsList <- CalcSws(swpGraph,randGraph)
    swpGamma  <-  transitivity(swpGraph, type="global", vids=NULL, weights=NULL)
    write(paste(step,'\t',HubCounts(FindHubs(runCount, hubThreshold, swpGraph)),
          '\t', swsList$Sws,'\t', swsList$swpPathLength,'\t',
          swsList$swpClustering,'\t', swsList$swpCC, '\t', swsList$Sws2 ),
          file= runLogOutput, append = TRUE, sep="," )

    # Print Degree Distribution Data
    PrintDegree(swpGraph, runCount, step)
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
    write(paste('seedVale: ', seedValue), file= outfileName, append = TRUE, sep=",")
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
    
    outfileName <- paste('starting_params.txt', sep="")
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

# Plots out the node degrees of a graph at each step.
PrintDegree <- function(swpGraph, runCount, step){
  degreeOutput = paste('run',runCount,'_DegreeLog.txt', sep="")
  d <- degree(swpGraph)
  cat(d, fill= 3*length(d), file=degreeOutput, sep=",", append = TRUE)
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
hubThreshold <- as.numeric(args[7]) # The threshold of the centrality score for determing a hub

# Number of runs
trialCount <- as.numeric(args[8])
timeSteps  <- as.numeric(args[9])

setwd(topFolder)


# Generate Directories for all trials
runCount <- 1

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
  notSWP      <- TRUE # true if the graphs are not swp
  notSWPCount <- 0
  while(notSWP){
    print("redo")
    notSWPCount <- notSWPCount + 1
    print(notSWPCount)
    swpGraph <- MakeSWPNetwork(dimension,size,nei,p)
    randGraph <- MakeRandNetwork(dimension, size, nei, swpGraph)
    if(CalcSws(swpGraph, randGraph)$Sws > 1) notSWP <- FALSE
    if(notSWPCount >= 1000){
	    write(paste('Could not generate SWP graph in ', notSWPCount, 
	  	            ' tries.'), file= 'failed.txt', append = TRUE, sep= ", ")
	    quit(save = "no")
      }
    }    
    
    # Run functions on Graphs
    # ------------------------
    hubMatrix <- FindHubs(runCount, hubThreshold, swpGraph)
   # CalcSws = CalcSws(swpGraph, randGraph)
    PrintGraphStats(runCount, swpGraph, randGraph, hubMatrix, dimension, size, nei, p,
                    hubThreshold)

    hubs_Model_run <- Run_Hubs_Model(runCount, swpGraph, randGraph, hubThreshold, 
                           timeSteps)



    # Increment for next run
    # ----------------------
    runCount <- runCount + 1

    # Make directory for degree printouts, and move them there
    if(runCount >= trialCount){
      system('mkdir DegreeLogs')
      system('mv *_DegreeLog.txt DegreeLogs')
    }

    setwd("..") # Go up a directory
}




print(warnings())
