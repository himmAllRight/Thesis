# Returns the number of hubs in the Matrix
HubCounts <- function(hubMatrix){
  count = sum(hubMatrix == 1)
  return(count)
}
