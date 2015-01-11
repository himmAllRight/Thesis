# Finds the hubs of a network.
FindHubs <- function(runCount, hubThreshold, swpGraph){
  hubScore  = hub.score(swpGraph) 
  hubValues = hubScore$vector      # Takes just values from hub score
  # Replaces all hubs with a 1, and other vertices with a 0.
  hubMatrix = replace(replace(hubValues,hubValues >= hubThreshold, 1),
                              hubValues < hubThreshold,0)  
  return(hubMatrix)
}
