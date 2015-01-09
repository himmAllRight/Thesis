library(igraph)
library(parallel)


# Set param loops
dimArray 	= c(4)
sizeArray	= c(3)
neiArray	= c(1)
pArray		= c(.15, .25, .35, .45)
thresholdArray	= c(1, 1.2)
nArray		= c(10, 20)
dArray		= c(2, 5)

runs 		= 30
steps		= 5000

runParamList = c()

# Build param comb list
