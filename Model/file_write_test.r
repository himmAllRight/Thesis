library(igraph)

## Model Parameters
dim              = 4	# Interger Constant, the demension of the starting lattice
size             = 3 	# The size of the lattice along each dimension
nei              = 1    # the neighborhood within which the verticies of the lattice will be connected
p                = 1   # the rewiring probabillity

make_graph <- function(dim, size, nei, p){
swpGraph <- watts.strogatz.game(dim,size,nei,p, loops = FALSE, multiple = FALSE)

    hubScore  = hub.score(swpGraph) # !! need to only get matrix from this
    hubMatrix = hubScore$vector
    return(hubMatrix)
}

hubMatrix = make_graph(dim,size,nei,p)

write(hubMatrix, file="test_output.txt", sep= "\n")