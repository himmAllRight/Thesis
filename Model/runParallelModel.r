library(igraph)
library(parallel)


# Set param loops
dimArray 	      = c(4, 3)
sizeArray	      = c(3, 4)
neiArray	      = c(1)
pArray		      = c(.15, .25, .35, .45)
thresholdArray	= c(1, 1.2)
nArray		      = c(10, 20)
dArray		      = c(2, 3, 4, 5, 6, 7, 8, 9, 10)

runs 		        = 30
steps		        = 5000

runParamList = c()

# Build param comb list ;; A terrible nested loop...

for(dim in dimArray){
  for(size in sizeArray){
    for(nei in neiArray){
      for(p in pArray){
        for(threshold in thresholdArray){
          for(n in nArray){
            for(d in dArray){
             print(sum(dim, size, nei, p, threshold, n, d, runs, steps) )
            }
          }
        }
      }
    }
  }
}
