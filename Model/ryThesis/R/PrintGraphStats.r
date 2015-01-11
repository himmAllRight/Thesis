PrintGraphStats <- function(runCount, swpGraph, randGraph, hubMatrix,dimension, size,
                            nei, p, hubThreshold){
  # Generate output file of each run in each run directory
  outfileName = "../cumulative_attributes.txt"   
    
  for (i in seq(from=1, to=2, by=1)){
    write('-----', file= outfileName, append = TRUE)
    write(paste('runCount: ', runCount), file= outfileName, append = TRUE, sep= ", ")
    write(paste('seedValue: ', seedValue), file= outfileName, append = TRUE, sep= ",")
    write(paste('dimension: ', dimension), file= outfileName, append = TRUE, sep= ", ")
    write(paste('Size: ',size), file= outfileName, append = TRUE, sep= ", ")
    write(paste('Nei: ', nei), file= outfileName, append = TRUE, sep= ", ")
    write(paste('p: ',p), file= outfileName, append = TRUE, sep= ", ")
    write(paste('hubThreshold: ', hubThreshold), file= outfileName,
          append = TRUE, sep= ", ")

    write('', file= outfileName, append = TRUE)
    write(paste("Orig. Generated Graph:"), file= outfileName, append = TRUE, sep=", ")
#    write(paste('swpGraph Vertice count: ', vcount(swp1)), file= outfileName,
#          append = TRUE, sep= ", ")
#    write(paste('swpGraph Edge count: ',ecount(swp1)), file= outfileName, 
#          append = TRUE, sep= ", ")
#    write(paste('swpGraph Sws: ', CalcSws(swp1, rand1)$Sws), file= outfileName, append = TRUE, 
#          sep= ", ")
    write(paste('swpGraph Hub count: ', sum(hubMatrix == 1)), file= outfileName,
          append = TRUE, sep= ", ")
    write('', file= outfileName, append = TRUE)

    write(paste("After graph cleaning:"), file=outfileName, append = TRUE, sep=", ")
    write(paste('swpGraph Vertice count: ', vcount(swpGraph)), file= outfileName,
          append = TRUE, sep= ", ")
    write(paste('swpGraph Edge count: ',ecount(swpGraph)), file= outfileName, 
          append = TRUE, sep= ", ")
    write(paste('swpGraph Sws: ', CalcSws(swpGraph, randGraph)$Sws), file= outfileName, append = TRUE, 
          sep= ", ")
    write(paste('swpGraph Hub count: ', sum(hubMatrix == 1)), file= outfileName,
          append = TRUE, sep= ", ")
    write('-----', file= outfileName, append = TRUE)
    write('', file= outfileName, append = TRUE)
    outfileName = paste('starting_params.txt', sep="")
    }
}
