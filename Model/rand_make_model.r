# Ryan Himmelwright ## Honors Thesis ## Make Model Script
library(igraph)
library(methods)
library(lattice)
library(base)
library(Matrix)

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
#  randGraph <- watts.strogatz.game(dimension,size,nei,1,loops = FALSE, multiple = FALSE)
  randGraph <- erdos.renyi.game(vcount(swpGraph),ecount(swpGraph), type="gnm",
                               directed = FALSE, loops = FALSE)
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
  # old
  swpGamma  <-  transitivity(swpGraph, type="global", vids=NULL, weights=NULL,
                            isolates="Nan")
  # New Clustering Coefficient Calc
  swpCC     <- transitivity(swpGraph, type='localaverageundirected', vids=NULL,
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

################################################################################
##############################  Models Functions  ##############################
################################################################################

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



################################################################################
############################## Printing Functions ##############################
################################################################################
PrintGraphStats <- function(runCount, swpGraph, randGraph, hubMatrix,dimension, size,
                            nei, p, hubThreshold){
  # Generate output file of each run in each run directory
  outfileName = "../cumulative_attributes.txt"   
    
  for (i in seq(from=1, to=2, by=1)){
    write('-----', file= outfileName, append = TRUE)
    write(paste('runCount: ', runCount), file= outfileName, append = TRUE, sep= ", ")
    write(paste('seedValue: ', seedValue), file= outfileName, append = TRUE, sep= ",")
    write(paste('dimension: ', dimension), file= outfileName, append = TRUE, sep= ", ")
    write(paste('Size: ',size), file= outfileName, append = TRUE, sep= ", ")
    write(paste('Nei: ', nei), file= outfileName, append = TRUE, sep= ", ")
    write(paste('p: ',p), file= outfileName, append = TRUE, sep= ", ")
    write(paste('hubThreshold: ', hubThreshold), file= outfileName,
          append = TRUE, sep= ", ")

    write('', file= outfileName, append = TRUE)
    write(paste("Orig. Generated Graph:"), file= outfileName, append = TRUE, sep=", ")
#    write(paste('swpGraph Vertice count: ', vcount(swp1)), file= outfileName,
#          append = TRUE, sep= ", ")
#    write(paste('swpGraph Edge count: ',ecount(swp1)), file= outfileName, 
#          append = TRUE, sep= ", ")
#    write(paste('swpGraph Sws: ', CalcSws(swp1, rand1)$Sws), file= outfileName, append = TRUE, 
#          sep= ", ")
    write(paste('swpGraph Hub count: ', sum(hubMatrix == 1)), file= outfileName,
          append = TRUE, sep= ", ")
    write('', file= outfileName, append = TRUE)

    write(paste("After graph cleaning:"), file=outfileName, append = TRUE, sep=", ")
    write(paste('swpGraph Vertice count: ', vcount(swpGraph)), file= outfileName,
          append = TRUE, sep= ", ")
    write(paste('swpGraph Edge count: ',ecount(swpGraph)), file= outfileName, 
          append = TRUE, sep= ", ")
    write(paste('swpGraph Sws: ', CalcSws(swpGraph, randGraph)$Sws), file= outfileName, append = TRUE, 
          sep= ", ")
    write(paste('swpGraph Hub count: ', sum(hubMatrix == 1)), file= outfileName,
          append = TRUE, sep= ", ")
    write('-----', file= outfileName, append = TRUE)
    write('', file= outfileName, append = TRUE)
    outfileName = paste('starting_params.txt', sep="")
    }
}

PlotGraph <- function(runCount,tag, swpGraph, randGraph, hubMatrix){
  # Color hubs in SWP plot.
  for (i in seq(from=1, to= length(hubMatrix), by=1)){
    if(hubMatrix[i] == 1){
      V(swpGraph)$color[i] = "green"
      }
      else{
        V(swpGraph)$color[i] = "cyan"
      }
  }
  # SWP Plot
  png(file=(paste("SWPplot",runCount,tag,".png",sep="")))
  plot(swpGraph, vertex.size=3, vertex.label=NA, edge.arrow.size=0)
  dev.off()
  # Rand Plot
  png(file="rand_plot1.png")
  plot(randGraph, vertex.size=3, vertex.label=NA, edge.arrow.size=0)
  dev.off()
}

# Plots out the node degrees of a graph at each step.
PrintDegree <- function(swpGraph, runCount, step){
  # Change to DegreeData folder
  setwd('DegreeData')

  # Make degree Matrix
  d <- degree(swpGraph)

  # Write to file
  degreeOutput = paste('run',runCount,'_DegreeLog.txt', sep="")
  cat(d, fill= 3*length(d), file=degreeOutput, sep=",", append = TRUE)

  setwd('..')   # Jump out of folder
}



# Plots out the degree distribution each step.
PrintDegreeDist <- function(swpGraph, runCount, step, timeSteps, degreeMax,
                            probMax){
  # Initialize data Folder
  folder = paste('run_',runCount, 'degreeDistData', sep = "" )

  # Change to Degree Distribution dir
  setwd('DegreeData')
  
  # If a sub-directory does not exit for current run, make it.
  system(paste('mkdir -p ', folder, sep = "" ))
  setwd(folder)     # Set working Directory to the Degree Run
  
  # Generate degree distribution matrix
  d <- degree.distribution(swpGraph)

  # Write to file
  degreeDistOutput = paste('run',runCount,'_step', step,'_DegreeDist.dat', sep="")
  for(i in seq(from=1, to= length(d), by=1)){
    write(paste(i,'\t',d[i]), file = degreeDistOutput, append = TRUE)
  }

  # If last step
  if(step == timeSteps){
    write(paste(degreeMax, probMax, sep="\n"), sep="\n", file = "windowInfo.txt")
  }

  setwd('../..')    # Back out of degree run directory

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
hubThreshold  = as.numeric(args[7]) # The threshold of the centrality score for determing a hub

# Number of runs
trialCount= as.numeric(args[8])
timeSteps = as.numeric(args[9])

setwd(topFolder)


# Generate Directories for all trials
runCount =1

for( i in seq(from=1, to= trialCount, by=1)){

  print(getwd()) # print current working directory
  
  print(paste('mkdir ',name, sep=""))
  system(paste('mkdir ', name, sep=""))
  setwd(paste(name, sep=""))

  system('mkdir DegreeData')

  print(getwd())

  #-----------------------------------------------
  #--------------- Model Sequence ----------------
  #-----------------------------------------------

  # Generate Graphs
  # ----------------
  notSWP      = TRUE # true if the graphs are not swp
  notSWPCount = 0
  while(notSWP){
    print("redo") # Re-generating Graph
    notSWPCount = notSWPCount + 1
    print(notSWPCount) # Print re-gen number
    
    # Make New Graphs
    swpGraph = MakeSWPNetwork(dimension,size,nei,p)
    randGraph = MakeRandNetwork(dimension, size, nei, swpGraph)

    if(CalcSws(swpGraph, randGraph)$Sws > 1) notSWP=FALSE
    if(notSWPCount >= 1000){
	    write(paste('Could not generate SWP graph in ', notSWPCount, 
	  	            ' tries.'), file= 'failed.txt', append = TRUE, sep= ", ")
	    quit(save = "no")
    }
  }    
    
    print("Start steps")

  # Clean Graphs
#  graphList   <- CleanGraphs(swpGraph, randGraph)
#  swp1        <- graphList$swp1      # Origonal swp for comparison
#  rand1       <- graphList$rand1     # Origonal rand for comparison
#  swpGraph    <- graphList$swpGraph  # Cleaned swp Graph
#  randGraph   <- graphList$randGraph # Cleaned rand Graph

  # Run functions on Graphs
  # ------------------------
  hubMatrix = FindHubs(runCount, hubThreshold, swpGraph)
  PrintGraphStats(runCount, swpGraph, randGraph, hubMatrix, dimension, size, nei, p,
                  hubThreshold)
  
  # Plot Graphs Before Model Runs
#  plotGraph = PlotGraph(runCount, "pre", swpGraph, randGraph, hubMatrix)

  # Run Model  
  rand_Model_Run = Run_Random_Model(runCount, swpGraph, randGraph, hubMatrix,
                                     timeSteps)
  # Plot Graphs After Model Runs
# plotGraph = PlotGraph(runCount, "post", swpGraph, randGraph, hubMatrix)


  # Increment for next run
  # ----------------------
  runCount = runCount + 1

  # Make directory for degree printouts, and move them there
  if(runCount >= trialCount){
    system('mkdir DegreeLogs')
    system('mv *_DegreeLog.txt DegreeLogs')
  }

  setwd("..") # Go up a directory
}
print(warnings())
