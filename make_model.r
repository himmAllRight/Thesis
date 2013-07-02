## Ryan Himmelwright
## Honors Thesis
## Make Model Script
## 5/31/13

library(igraph)

#################################################
############### Defined Functions ###############
#################################################

makeSWPNetwork <- function(nodeCount){
g <- watts.strogatz.game(1,nodeCount,2,.5, loops = FALSE, multiple = FALSE)
return(g)
}


#################################################
############## Execition Code ###################
#################################################

## Model Parameters
nodeCount = 25

## Execute
swpGraph = makeSWPNetwork(nodeCount)

## Plot
png(file="SWP_plot1.png")
plot(swpGraph)


