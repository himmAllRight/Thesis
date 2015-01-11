PlotGraph <- function(runCount,tag, swpGraph, randGraph, hubMatrix){
  # Color hubs in SWP plot.
  for (i in seq(from=1, to= length(hubMatrix), by=1)){
    if(hubMatrix[i] == 1){
      V(swpGraph)$color[i] = "green"
      }
      else{
        V(swpGraph)$color[i] = "cyan"
      }
  }
  # SWP Plot
  png(file=(paste("SWPplot",runCount,tag,".png",sep="")))
  plot(swpGraph, vertex.size=3, vertex.label=NA, edge.arrow.size=0)
  dev.off()
  # Rand Plot
  png(file="rand_plot1.png")
  plot(randGraph, vertex.size=3, vertex.label=NA, edge.arrow.size=0)
  dev.off()
}
