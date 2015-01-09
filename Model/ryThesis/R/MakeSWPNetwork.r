# Function to generate Small World Graph
MakeSWPNetwork <- function(dimension,size,nei,p){                                              
  swpGraph <- watts.strogatz.game(dimension,size,nei,p, loops = FALSE, multiple = FALSE)       
  return(swpGraph)                                                                             
}
