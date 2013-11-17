library(igraph)

g <- watts.strogatz.game(2,25,1,.25)
plot(g)

# Finds the hubs of a network.
FindHubs <- function(hubThreshold, swpGraph){
  hubScore  = hub.score(swpGraph)
  hubValues = hubScore$vector      # Takes just values from hub score
  # Replaces all hubs with a 1, and other vertices with a 0.
  hubMatrix = replace(replace(hubValues,hubValues >= hubThreshold, 1),
                              hubValues < hubThreshold,0) 
  return(hubMatrix)
}


# Returns two lists containing all the hub-hub connections.
HubHub <- function(swpGraph, hubs){
  hubhub1 <- c()
  hubhub2 <- c()

  for(hubN in hubs){
    adjHubs <- intersect(unlist(get.adjlist(swpGraph)[hubN]), hubs)
    if(length(adjHubs) > 0){
      for( m in seq(from=1, to=length(adjHubs), by=1)){

print(paste("adj m: ", adjHubs[m]))
        hubhub1 <- c(hubhub1, hubN)
        hubhub2 <- c(hubhub2, adjHubs[m])
      }
    }
  }

  hubhub <- list("hubhub1" = hubhub1, "hubhub2"=hubhub2)
  return(hubhub)
}

hubMatrix <- FindHubs(.80, g)
hubs <- (which(hubMatrix %in% 1))

hubhub  <- HubHub(g,hubs)
hubhub1 <- hubhub$hubhub1
hubhub2 <- hubhub$hubhub2

print("hubhub")
print(hubhub)
print("hubhub1")
print(hubhub1)
print("hubhub2")
print(hubhub2)

if( length(hubhub1) >= 1){
  print("1")
}else if ( length(hubhub1)< 2 ){
  print("2")
}
