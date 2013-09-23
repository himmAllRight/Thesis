
## Ryan Himmelwright
## Honors Thesis
## Make Model Script
library(igraph)
library(base)

#################################################
############### Defined Functions ###############
#################################################

# Function to Generate Small World Graph
makeSWPNetwork <- function(dim,size,nei,p){
    swpGraph <- watts.strogatz.game(dim,size,nei,p, loops = FALSE, multiple = FALSE)
return(swpGraph)
}

# Function to Generate an Erdos-Renyi random graph
makeRandNetwork <- function(dim, size, nei, swpGraph){
#    randGraph <- erdos.renyi.game((size^dim), ecount(swpGraph), type=c("gnm"), directed = FALSE, loops = FALSE)
    # Try to make a random graph with the watts.strogatz.game function
     randGraph <- watts.strogatz.game(dim,size,nei,1,loops = FALSE, multiple = FALSE)

return(randGraph)
}

# Finds the hubs of a network.
findHubs <- function(runCount, hubThreshold, swpGraph){
    hubScore  = hub.score(swpGraph) # !! need to only get matrix from this
    hubValues = hubScore$vector
    hubMatrix = replace(replace(hubValues,hubValues >= hubThreshold, 1), hubValues < hubThreshold,0)
    return(hubMatrix)
}

# Returns the number of hubs in the Matrix
hubCounts <- function(hubMatrix){
    count = sum(hubMatrix == 1)
    print(count)
    return(count)
}


# Calculates S^WS for the network.
calc_Sws <- function(swpGraph, randGraph){
#    print("transitivity")
    swpGamma  =  transitivity(swpGraph, type="global", vids=NULL, weights=NULL, isolates="zero") # calculates colustering coefficient of the swp graph.
    randGamma = transitivity(randGraph, type="global", vids=NULL, weights=NULL, isolates="zero") # calculates colustering coefficient of the rand graph.
    Gamma = (swpGamma/randGamma) # combines them to get the Gamma value.
    swpLambda = average.path.length(swpGraph)   # Calculates the mean minmal path length for swp graph
    randLambda = average.path.length(randGraph) # Calculates the mean minmal path length corresponding rand graph
    Lambda = (swpLambda / randLambda)           # Combines to get the ratio Lambda value

    Sws = (Gamma/Lambda) # Calculates S^WS from the ratio.
    print(Sws)
    return(Sws)
}

# Might forget about this for a bit to focus on more important parts of the model.
# It might slow things down alot if I code it by myself by hand. (high complexity)
# Calculate S^delta
calc_Sdelta <- function(swpGraph, randGraph){
    # Calculate LamdaG (LamdaG = Lg / Lrand)
    Lg     = average.path.length(swpGraph, directed = FALSE)
    Lrand  = average.path.length(randGraph, directed = FALSE)
    LamdaG = ( Lg / Lrand )
    
    print("Lamda G")
    print(Lg)
    print(Lrand)
    print(LamdaG)
    
    
    # Calculate GammaDeltaG ( GammaDeltaG = cDeltaG / cDeltaRand )
    
    # Find a way to determine the number of triangles in a graph. ( and number of paths of length 2?)
    print("Triangle Stuff")
    # Generates an adjacency matrix of the graph that can be used to find all the triangles.
    swpMatrix = get.adjacency(swpGraph, type=c("upper"))  
    print(swpMatrix)
}

print_graph_stats <- function(runCount, swpGraph, swp_Sws, randGraph, hubMatrix){
    outfileName = "../cumulative_attributes.txt"   # Generate output file of each run in each run directory
    
    for (i in seq(from=1, to=2, by=1)){
        write('runCount: ', file= outfileName, append = TRUE, sep= ", ")
        write(runCount, file= outfileName, append = TRUE, sep= ", ")
#        write('swpGraph Vertice count: ', file= outfileName, append = TRUE, sep= ", ")
#        write(vcount(swpGraph), file= outfileName, append = TRUE,  sep= ", ")
#        write('swpGraph Edge count: ', file= outfileName, append = TRUE, sep= ", ")
#        write(ecount(swpGraph), file= outfileName, append = TRUE,  sep= ", ")
        write('swpGraph Sws: ', file= outfileName, append = TRUE, sep= ", ")
        write(swp_Sws , file= outfileName, append = TRUE,  sep= ", ")
        write('swpGraph Hub count: ', file= outfileName, append = TRUE, sep= ", ")
        count = sum(hubMatrix == 1)
        write(count, file= outfileName, append = TRUE,  sep= ", ")

        write('', file= outfileName, append = TRUE)

        outfileName = paste('output_run',i,'.txt', sep="")
    }
}

plot_Graph <- function(runCount, swpGraph, randGraph, hubMatrix){
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
    png(file=(paste("SWP_plot",runCount,".png",sep="")))
    plot(swpGraph)
    png(file="rand_plot1.png")
    plot(randGraph)
}


#################################################
############## Execition Code ###################
#################################################
trialCount= 10 

## Model Parameters
dim              = 4	# Interger Constant, the demension of the starting lattice
size             = 3 	# The size of the lattice along each dimension
nei              = 1    # the neighborhood within which the verticies of the lattice will be connected
p                = 1   # the rewiring probabillity
hubThreshold     = 0.8  # The threshold of the centrality score for determing a hub


# Generate Directories for all trials
for (i in seq(from=1, to= trialCount, by=1)){
    print(paste('mkdir model_run',i, sep=""))
    system(paste('mkdir model_run',i, sep=""))
}


# itteration of runs
runList  = Sys.glob("model_run*")

# Loop through each run
runCount =1
for( currDir in runList){
    print(currDir)
    setwd(currDir) # enter directory
    print(getwd()) # print current working directory
    #-----------------------------------------------
    #--------------- Model Sequence ----------------
    #-----------------------------------------------
    # What to run for each model run

    # Generate small world graph with corresponding random graph.
    not_swp = TRUE # true if the graphs are not swp
    while(not_swp){
        print("redo")
        swpGraph = makeSWPNetwork(dim,size,nei,p)
        randGraph = makeRandNetwork(dim, size, nei, swpGraph)
        if(calc_Sws(swpGraph, randGraph) > 1) not_swp = FALSE
    }

    #Sdelta = calc_Sdelta(swpGraph, randGraph)
    hubMatrix = findHubs(runCount, hubThreshold, swpGraph)
    swp_Sws = calc_Sws(swpGraph, randGraph)
    print_graph_stats(runCount, swpGraph, swp_Sws, randGraph, hubMatrix)
#    plotGraph = plot_Graph(runCount, swpGraph, randGraph, hubMatrix)

#    -----------------------------------------------
#    -----------------------------------------------
    print(hubMatrix)
    print("runcount")
    print(runCount)
    runCount = runCount + 1
    setwd("..") # Go up a directory
}
print(warnings())
