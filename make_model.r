## Ryan Himmelwright
## Honors Thesis
## Make Model Script
## 5/31/13

library(igraph)

#################################################
############### Defined Functions ###############
#################################################

makeSWPNetwork <- function(dim,nodeCount){
g <- watts.strogatz.game(dim,nodeCount,2,.5, loops = FALSE, multiple = FALSE)
return(g)
}


#################################################
############## Execition Code ###################
#################################################

## Model Parameters
dim       = 1
nodeCount = 25

## Execute
swpGraph = makeSWPNetwork(dim,nodeCount)

## Plot
png(file="SWP_plot1.png")
plot(swpGraph)


