library(igraph)
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
