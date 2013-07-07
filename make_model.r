## Ryan Himmelwright
## Honors Thesis
## Make Model Script
## 5/31/13

library(igraph)

#################################################
############### Defined Functions ###############
#################################################

makeSWPNetwork <- function(dim,size,nei,p){
g <- watts.strogatz.game(dim,size,nei,p, loops = FALSE, multiple = FALSE)

# print(hub.score(g))

return(g)
}


#################################################
############## Execition Code ###################
#################################################

## Model Parameters
dim       = 1	# Interger Constant, the demension of the starting lattice
size      = 2 	# The size of the lattice along each dimension
nei       = 2   # the neighborhood within which the verticies of the lattice will be connected
p         = .5  # the rewiring probabillity


## Execute
swpGraph = makeSWPNetwork(dim,size,nei,p)
## Plot
png(file="SWP_plot1.png")
plot(swpGraph)


