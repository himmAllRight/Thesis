## Ryan Himmelwright
## Honors Thesis
## Make Model Script
## 8/28/2013

library(igraph)

#################################################
############### Defined Functions ###############
#################################################

# Function to Generate Small World Graph
makeSWPNetwork <- function(dim,size,nei,p){
    swpGraph <- watts.strogatz.game(dim,size,nei,p, loops = FALSE, multiple = FALSE)

    print(size^dim)
    print("Vertices Count:")
    print(vcount(swpGraph))
    print("Edge Count:")
    print(ecount(swpGraph))
    print("Degree:")
    print(degree(swpGraph))

return(swpGraph)
}

# Function to Generate an Erdos-Renyi random graph
makeRandNetwork <- function(dim, size, swpGraph){
    randGraph <- erdos.renyi.game((size^dim), ecount(swpGraph), type=c("gnm"), directed = FALSE, loops = FALSE)
    
    print(size^dim)
    print("Vertices Count:")
    print(vcount(randGraph))
    print("Edge Count:")
    print(ecount(randGraph))
    print("Degree:")
    print(degree(randGraph))
return(randGraph)
}

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
}

#################################################
############## Execition Code ###################
#################################################

## Model Parameters
dim       = 3	# Interger Constant, the demension of the starting lattice
size      = 4 	# The size of the lattice along each dimension
nei       = 2   # the neighborhood within which the verticies of the lattice will be connected
p         = .5  # the rewiring probabillity


## Execute
swpGraph = makeSWPNetwork(dim,size,nei,p)
randGraph = makeRandNetwork(dim, size, swpGraph)
Sdelta = calc_Sdelta(swpGraph, randGraph)
## Plot
png(file="SWP_plot1.png")
plot(swpGraph)
png(file="rand_plot1.png")
plot(randGraph)
