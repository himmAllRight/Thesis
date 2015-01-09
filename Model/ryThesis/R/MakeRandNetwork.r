#Functionn to Generate an Erdos-Renyi random graph                                             
MakeRandNetwork <- function(nodeCount, edgeCount){
  randGraph <- erdos.renyi.game(nodeCount, edgeCount, type="gnm", directed = FALSE, loops = FALSE)
  return(randGraph)
}
