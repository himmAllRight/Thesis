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

  	# Add new data cumulative lists
  	hubCountsA	<- c(hubCountsA, mean(dataMatrix[,2][1:windowSize]))
  	hubCountsB	<- c(hubCountsB, mean(dataMatrix[,2][((totalSteps - windowSize)+1):totalSteps]))

  	Sws2A		<- c(Sws2A, mean(dataMatrix[,7][1:windowSize]))
  	Sws2B		<- c(Sws2B, mean(dataMatrix[,7][((totalSteps - windowSize)+1):totalSteps]))

  	pathLengthA	<- c(pathLengthA, mean(dataMatrix[,4][1:windowSize]))
  	pathLengthB	<- c(pathLengthB, mean(dataMatrix[,4][((totalSteps - windowSize)+1):totalSteps]))

  	ccA		<- c(ccA, mean(dataMatrix[,6][1:windowSize]))
  	ccB		<- c(ccB, mean(dataMatrix[,6][((totalSteps - windowSize)+1):totalSteps]))

  	#print(dataMatrix[,2][1:windowSize])
  	#print(dataMatrix[,2][((totalSteps - windowSize)+1):totalSteps])

  }


# Stats
# Run statistics on List
system('mkdir Statistics')


# Hubs
a <- unlist(t.test(hubCountsA,hubCountsB))
output = paste('t = ',a[1],"\n",'df = ', a[2], '\n','p-vale = ', a[3], '\n','95 percent confidence interval: ', a[4],'  ', a[5], '\n','mean of x: ', a[6], '\n','mean of y: ', a[7])
write(output,file="Statistics/hubCount_TTests.txt")


# Sws2
a <- unlist(t.test(Sws2A,Sws2B))
output = paste('t = ',a[1],"\n",'df = ', a[2], '\n','p-vale = ', a[3], '\n','95 percent confidence interval: ', a[4],'  ', a[5], '\n','mean of x: ', a[6], '\n','mean of y: ', a[7])
write(output,file="Statistics/Sws2_TTests.txt")


# Path Length
a <- unlist(t.test(pathLengthA,pathLengthB))
output = paste('t = ',a[1],"\n",'df = ', a[2], '\n','p-vale = ', a[3], '\n','95 percent confidence interval: ', a[4],'  ', a[5], '\n','mean of x: ', a[6], '\n','mean of y: ', a[7])
write(output,file="Statistics/pathLength_TTests.txt")

# Clustering Coefficient
a <- unlist(t.test(ccA,ccB))
output = paste('t = ',a[1],"\n",'df = ', a[2], '\n','p-vale = ', a[3], '\n','95 percent confidence interval: ', a[4],'  ', a[5], '\n','mean of x: ', a[6], '\n','mean of y: ', a[7])
write(output,file="Statistics/cc_TTests.txt")


  setwd("..")  # Move up a directory
}
