# Functions
MakeDataMatrix <- function(fileName){
  print(fileName)
  dataMatrix <- read.table(fileName, header = TRUE, sep="\t")
  
  return(dataMatrix)
}


# Parameters
windowSize = 500
totalSteps = 2500

paramList = Sys.glob("ModelRun*")
count = 1
# Loops through each folder containing runs of a different parameter set.
for(paramSet in paramList){
  print(paramSet)
  setwd(paramSet) 	# Enter parameter set


  # Define cumulative Lists
  # Hub Counts
  hubCountsA	<- c()
  hubCountsB	<- c()

  # Sws2
  Sws2A			<- c()
  Sws2B			<- c()

  # Average Path Length
  pathLengthA	<- c()
  pathLengthB	<- c()

  # Cluster Coefficient
  ccA			<- c()
  ccB			<- c()


  #For each run in parameter set
  runsList = Sys.glob("run*")
  for( run in runsList){
  	print(run)

  	dataMatrix  <- MakeDataMatrix(run)

  	# Add new data to cumulative Hubs Lists
  	hubCountsA	<- c(hubCountsA, dataMatrix[,2][1:windowSize])
  	hubCountsB	<- c(hubCountsB, dataMatrix[,2][((totalSteps - windowSize)+1):totalSteps])

  	#print(dataMatrix[,2][1:windowSize])
  	#print(dataMatrix[,2][((totalSteps - windowSize)+1):totalSteps])

  }

print("hubCountsA")
#print(hubCountsA)
print(length(hubCountsA))

print("hubCountsB")
#print(hubCountsB)
print(length(hubCountsB))

# Run statistics on List
print(t.test(hubCountsA,hubCountsB))


  setwd("..")  # Move up a directory
}