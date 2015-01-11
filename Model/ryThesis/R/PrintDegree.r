# Plots out the node degrees of a graph at each step.
PrintDegree <- function(swpGraph, runCount, step){
  # Change to DegreeData folder
  setwd('DegreeData')

  # Make degree Matrix
  d <- degree(swpGraph)

  # Write to file
  degreeOutput = paste('run',runCount,'_DegreeLog.txt', sep="")
  cat(d, fill= 3*length(d), file=degreeOutput, sep=",", append = TRUE)

  setwd('..')   # Jump out of folder
}
