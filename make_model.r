## Ryan Himmelwright
## Honors Thesis
## Make Model Script
## 8/27/2013

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
calc_Selta <- function(swpGraph, randGraph){
    
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
## Plot
png(file="SWP_plot1.png")
plot(swpGraph)
png(file="rand_plot1.png")
plot(randGraph)
