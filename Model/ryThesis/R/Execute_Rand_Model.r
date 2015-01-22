Execute_Random_Model <- function(topFolder, name, dimension, size, nei, p, hubThreshold, trialCount, timesteps, n = 10, d= 2){

  setwd(topFolder)


  # Generate Directories for all trials
  runCount =1

  for( i in seq(from=1, to= trialCount, by=1)){

    print(getwd()) # print current working directory
        
    print(paste('mkdir ',name, sep=""))
    system(paste('mkdir ', name, sep=""))
    setwd(paste(name, sep=""))

    system('mkdir DegreeData')

    print(getwd())
    #-----------------------------------------------
    #--------------- Model Sequence ----------------
    #-----------------------------------------------

    # Generate Graphs
    # ----------------
    notSWP      = TRUE # true if the graphs are not swp
    notSWPCount = 0
    while(notSWP){
      print("redo") # Re-generating Graph
      notSWPCount = notSWPCount + 1
      print(notSWPCount) # Print re-gen number
            
      # Make New Graphs
      swpGraph  <- MakeClusterGraph(dimension, size, nei, p, n, d)
      #swpGraph  <- MakeSWPNetwork(dimension,size,nei,p)
      randGraph <- MakeRandNetwork(vcount(swpGraph), ecount(swpGraph))
            

      if(CalcSws(swpGraph, randGraph)$Sws > 1) notSWP=FALSE
      if(notSWPCount >= 1000){
        write(paste('Could not generate SWP graph in ', notSWPCount, 
	           ' tries.'), file= 'failed.txt', append = TRUE, sep= ", ")
        quit(save = "no")
       }
     }    
        
    print("Start steps")

    # Clean Graphs
    #  graphList   <- CleanGraphs(swpGraph, randGraph)
  #  swp1        <- graphList$swp1      # Origonal swp for comparison
  #  rand1       <- graphList$rand1     # Origonal rand for comparison
  #  swpGraph    <- graphList$swpGraph  # Cleaned swp Graph
  #  randGraph   <- graphList$randGraph # Cleaned rand Graph
  # Run functions on Graphs
  # ------------------------
    hubMatrix = FindHubs(runCount, hubThreshold, swpGraph)
    PrintGraphStats(runCount, swpGraph, randGraph, hubMatrix, dimension, size, nei, p,
                    hubThreshold)
        
    # Plot Graphs Before Model Runs
  #  plotGraph = PlotGraph(runCount, "pre", swpGraph, randGraph, hubMatrix)

    # Run Model  
    rand_Model_Run = Run_Random_Model(runCount, swpGraph, randGraph, hubMatrix,
                                      timeSteps)
    # Plot Graphs After Model Runs
  # plotGraph = PlotGraph(runCount, "post", swpGraph, randGraph, hubMatrix)


    # Increment for next run
    # ----------------------
    runCount = runCount + 1

    # Make directory for degree printouts, and move them there
    if(runCount >= trialCount){
      system('mkdir DegreeLogs')
      system('mv *_DegreeLog.txt DegreeLogs')
    }

    setwd("..") # Go up a directory
  }
  print(warnings())
}
