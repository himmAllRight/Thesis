# Plots out the degree distribution each step.
PrintDegreeDist <- function(swpGraph, runCount, step, timeSteps, degreeMax,
                            probMax){
  # Initialize data Folder
  folder = paste('run_',runCount, 'degreeDistData', sep = "" )

  # Change to Degree Distribution dir
  setwd('DegreeData')
  
  # If a sub-directory does not exit for current run, make it.
  system(paste('mkdir -p ', folder, sep = "" ))
  setwd(folder)     # Set working Directory to the Degree Run
  
  # Generate degree distribution matrix
  d <- degree.distribution(swpGraph)

  # Write to file
  degreeDistOutput = paste('run',runCount,'_step', step,'_DegreeDist.dat', sep="")
  for(i in seq(from=1, to= length(d), by=1)){
    write(paste(i,'\t',d[i]), file = degreeDistOutput, append = TRUE)
  }

  # If last step
  if(step == timeSteps){
    write(paste(degreeMax, probMax, sep="\n"), sep="\n", file = "windowInfo.txt")
  }

  setwd('../..')    # Back out of degree run directory

}
