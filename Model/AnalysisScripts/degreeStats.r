# Plots the degree min, max, mean, and std of the degrees at each step.
plotDegreeStats <- function(degreeLogFile){
  print(paste("In plotDegreeStats function: ",degreeLogFile))
  
  con <- file(degreeLogFile, open = "r")
dataMatrix <- read.csv(degreeLogFile, header = FALSE, sep=",")
  # Stats Lists

print(dataMatrix)
  
}


# EXECUTION CODE #
##################
# Set currDir to directory the script is run from.
currDir = getwd()
setwd(currDir)

paramList = Sys.glob("ModelRun*")
count = 1

# Loops through each folder containing runs of a different parameter set.
for(paramSet in paramList){
  print(paramSet)
  setwd(paramSet)
  setwd("DegreeData")

  # Get Degree Logs
  runDegreeLog = Sys.glob("*DegreeLog.txt")

  for(DegreeLog in runDegreeLog){
  	print(DegreeLog)

  	# For each log, read file and plot range + std
  	plotDegreeStats(DegreeLog)

  }

}