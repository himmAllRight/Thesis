## Ryan Himmelwright
## Honors Thesis
## Make Model Script
library(igraph)


#################################################
############### Defined Functions ###############
#################################################

# Function to Generate Small World Graph
makeSWPNetwork <- function(dim,size,nei,p){
    swpGraph <- watts.strogatz.game(dim,size,nei,p, loops = FALSE, multiple = FALSE)

#    print(size^dim)
#    print("Vertices Count:")
#    print(vcount(swpGraph))
#    print("Edge Count:")
#    print(ecount(swpGraph))
#    print("Degree:")
#    print(degree(swpGraph))

return(swpGraph)
}

# Function to Generate an Erdos-Renyi random graph
makeRandNetwork <- function(dim, size, swpGraph){
    randGraph <- erdos.renyi.game((size^dim), ecount(swpGraph), type=c("gnm"), directed = FALSE, loops = FALSE)
#    print("rand")
#    print(size^dim)
#    print("Vertices Count:")
#    print(vcount(randGraph))
#    print("Edge Count:")
#    print(ecount(randGraph))
#    print("Degree:")
#    print(degree(randGraph))
return(randGraph)
}

# Finds the hubs of a network.
findHubs <- function(runCount, hubThreshold, swpGraph){
#    print("hub scores")
    hubScore  = hub.score(swpGraph) # !! need to only get matrix from this
    hubMatrix = hubScore$vector
#    print(hubMatrix)
    hubcount = 0
    for (i in seq(from=1, to= length(hubMatrix), by=1)){
        if (hubMatrix[i] >= hubThreshold){
            hubMatrix[i] = 1
            hubcount = hubcount + 1
        }
        else{
            hubMatrix[i] = 0
        }
    }

    return(hubMatrix)
    # Once we have a matrix of the hubs, add a for loop to change each one
    # to a defining color such as red, and return the graph so it can be printed
    # with the others.
}

# Calculates S^WS for the network.
calc_Sws <- function(swpGraph, randGraph){
    print("transitivity")
    swpGamma  =  transitivity(swpGraph, type="global", vids=NULL, weights=NULL, isolates="zero") # calculates colustering coefficient of the swp graph.
    randGamma = transitivity(randGraph, type="global", vids=NULL, weights=NULL, isolates="zero") # calculates colustering coefficient of the rand graph.
    Gamma = (swpGamma/randGamma) # combines them to get the Gamma value.
    print(swpGamma)
    print(randGamma)
    print(Gamma)

    print("path length")
    swpLambda = average.path.length(swpGraph)   # Calculates the mean minmal path length for swp graph
    randLambda = average.path.length(randGraph) # Calculates the mean minmal path length corresponding rand graph
    Lambda = (swpLambda / randLambda)           # Combines to get the ratio Lambda value
    print(swpLambda)
    print(randLambda)
    print(Lambda)

    print("S^WS")
    Sws = (Gamma/Lambda) # Calculates S^WS from the ratio.
    print(Sws)
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

## Model Parameters
dim              = 3	# Interger Constant, the demension of the starting lattice
size             = 4 	# The size of the lattice along each dimension
nei              = 1    # the neighborhood within which the verticies of the lattice will be connected
p                = .7   # the rewiring probabillity

## Other Parameters
hubThreshold     = 0.8  # The threshold of the centrality score for determing a hub
trialCount= 5


# Generate Directories for all trials
for (i in seq(from=1, to= trialCount, by=1)){
    print(paste('mkdir model_run',i, sep=""))
    system(paste('mkdir model_run',i, sep=""))
}

# itteration of runs
setwd("~/Dropbox/School/2013-2014/Thesis/Model/.")
runList  = Sys.glob("model_run*")

# Loop through each run
runCount =1
for( currDir in runList){
    setwd(currDir) # enter directory
    print(getwd())
#    -----------------------------------------------
#    --------------- Model Sequence ----------------
#    -----------------------------------------------
    # What to run for each model run
    swpGraph = makeSWPNetwork(dim,size,nei,p)
    randGraph = makeRandNetwork(dim, size, swpGraph)
    #Sdelta = calc_Sdelta(swpGraph, randGraph)
    hubMatrix = findHubs(runCount, hubThreshold, swpGraph)
    Sws = calc_Sws(swpGraph, randGraph)
    plotGraph = plot_Graph(runCount, swpGraph, randGraph, hubMatrix)

#    -----------------------------------------------
#    -----------------------------------------------
    print("runcount")
    print(runCount)
    runCount = runCount + 1
    setwd("..") # Go up a directory
}

