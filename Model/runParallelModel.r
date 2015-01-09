library(igraph)
require(parallel)


# Set param loops
dimArray 	      <- c(4, 3)
sizeArray	      <- c(3, 4)
neiArray	      <- c(1)
pArray		      <- c(.15, .25, .35, .45)
thresholdArray	<- c(1, 1.2)
nArray		      <- c(10, 20)
dArray		      <- c(2, 3, 4, 5, 6, 7, 8, 9, 10)

runs 		        <- 30
steps		        <- 5000


paramList <- list(dimArray, sizeArray, neiArray, pArray, thresholdArray, nArray, dArray)
print(expand.grid(paramList))  



