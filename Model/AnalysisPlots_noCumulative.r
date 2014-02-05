# Analysis script to make plots and do statistics on thesis data.

library(stringr)

print("Debug1")

MakeDataMatrix <- function(fileName){
  print(fileName)
  dataMatrix <- read.table(fileName, header = TRUE, sep="\t")
  
  return(dataMatrix)
}
print("Debug2")
# Plots the Sws data for each individual run, and adds it the the cumulative
# Sws data.frame for the cumulative plot.
PlotSws <- function(dataMatrix, filename, cumulativeSws, runCount){
  plotName = paste("SwsPlot_", unlist(str_split(filename,"_"))[1], ".png", sep="") 
  png(plotName) # make file
  plot(dataMatrix$step, dataMatrix$Sws, type="o") # plot line

  # Regression Line
  res=lm(dataMatrix$Sws~dataMatrix$step)
  abline(res, col='red', lwd='3')
  
  dev.off() # close file

  # Cumulative Data Generation
  cumulativeSws[,runCount] <- dataMatrix$Sws # add data to cumulative dataFrame
  return(cumulativeSws)
}
print("Debug3")
# Plots cumulative Sws plot.
PlotCumulativeSws <- function(cumulativeSws){
  plotName = paste("CumulativeSwsPlot.png")
  png(plotName) # Make file

  # Plot Data
  plot(cumulativeSws[,1], type='l', col="red") # Plots first line
  # loops through each run Data.
  for( i in seq(from=2, to=ncol(cumulativeSws), by=1)){
    lines(cumulativeSws[,i], col="red") # adds line for other runs
  }

  dev.off() # close file
}
print("Debug4")

PlotPathLength <- function(dataMatrix, filename, cumulativePathLength, runCount){
  plotName = paste("PathLengthPlot_", unlist(str_split(filename,"_"))[1], ".png", sep="") 
  png(plotName) # make file
  
  plot(dataMatrix$step, dataMatrix$avg_Path_Length, type="o") # plot line

  # Regression Line
  res=lm(dataMatrix$avg_Path_Length~dataMatrix$step)
  abline(res, col='red', lwd='3')

  dev.off()  # close file

 # Cumulative Data Generation
  cumulativePathLength[,runCount] <- dataMatrix$avg_Path_Length # add data to cumulative dataFrame
  return(cumulativePathLength)
}
print("Debug5")
# Plots cumulative PathLength plot.
PlotCumulativePathLength <- function(cumulativePathLength){
  plotName = paste("CumulativePathLengthPlot.png")
  png(plotName) # Make file

  # Plot Data
  plot(cumulativePathLength[,1], type='l', col="red") # Plots first line
  # loops through each run Data.
  for( i in seq(from=2, to=ncol(cumulativePathLength), by=1)){
    lines(cumulativePathLength[,i], col="red") # adds line for other runs
  }

  dev.off() # close file
}
print("Debug6")

PlotClustering <- function(dataMatrix, filename, cumulativeClustering, runCount){
  plotName = paste("TransitivityPlot_", unlist(str_split(filename,"_"))[1], ".png", sep="") 
  png(plotName) # make file
  
  plot(dataMatrix$step, dataMatrix$Transitivity, type="o") # plot line

  # Regression Line
  res=lm(dataMatrix$Transitivity~dataMatrix$step)
  abline(res, col='red', lwd='3')

  dev.off()  # Close File

 # Cumulative Data Generation
  cumulativeClustering[,runCount] <- dataMatrix$Transitivity # add data to cumulative dataFrame
  return(cumulativeClustering)
}
print("Debug7")
# Plots cumulative Clustering plot.
PlotCumulativeClustering <- function(cumulativeClustering){
  plotName = paste("CumulativeTransitivityPlot.png")
  png(plotName) # Make file

  # Plot Data
  plot(cumulativeClustering[,1], type='l', col="red") # Plots first line
  # loops through each run Data.
  for( i in seq(from=2, to=ncol(cumulativeClustering), by=1)){
    lines(cumulativeClustering[,i], col="red") # adds line for other runs
  }

  dev.off() # close file
}
print("Debug8")
PlotHubCount <- function(dataMatrix, filename, cumulativeHubCount, runCount){
  plotName = paste("HubCountPlot_", unlist(str_split(filename,"_"))[1], ".png", sep="") 
  png(plotName)
  
  plot(dataMatrix$step, dataMatrix$hubCount, type="o")

  # Regression Line
  res=lm(dataMatrix$hubCount~dataMatrix$step)
  abline(res, col='red', lwd='3')

  dev.off()

  # Cumulative Data Generation
  cumulativeHubCount[,runCount] <- dataMatrix$hubCount # add data to cumulative dataFrame
  return(cumulativeHubCount)  
}
print("Debug9")
# Plots cumulative Hub Count.
PlotCumulativeHubCount <- function(cumulativeHubCount){
  plotName = paste("CumulativeHubCountPlot.png")
  png(plotName) # Make file

  # Plot Data
  plot(cumulativeHubCount[,1], type='l', col="red") # Plots first line
  # loops through each run Data.
  print(paste("hubcount ncol: ", ncol(cumulativeHubCount)))
  for( i in seq(from=2, to=ncol(cumulativeHubCount), by=1)){
    lines(cumulativeHubCount[,i], col="red") # adds line for other runs
  }

  dev.off() # close file
}
print("Debug10")
# Plots new Clustering Coefficient
PlotCC <- function(dataMatrix, filename, cumulativeCC, runcount){
  plotName = paste("CCPlot_", unlist(str_split(filename,"_"))[1], ".png", sep="") 
  png(plotName)
  
  plot(dataMatrix$step, dataMatrix$Clustering, type="o")

  # Regression Line
  res=lm(dataMatrix$Clustering~dataMatrix$step)
  abline(res, col='red', lwd='3')

  dev.off()

  # Cumulative Data Generation
  cumulativeCC[,runCount] <- dataMatrix$Clustering # add data to cumulative dataFrame
  return(cumulativeCC)  
}
print("Debug11")
# Plots cumulative CC.
PlotCumulativeCC <- function(cumulativeCC){
  plotName = paste("CumulativeCCPlot.png")
  png(plotName) # Make file

  # Plot Data
  plot(cumulativeCC[,1], type='l', col="red") # Plots first line
  # loops through each run Data.
  print(paste("cumulativeCC ncol: ", ncol(cumulativeCC)))
  for( i in seq(from=2, to=ncol(cumulativeCC), by=1)){
    lines(cumulativeCC[,i], col="red") # adds line for other runs
  }

  dev.off() # close file
}
print("Debug12")
# Plots new Sws2
PlotSws2 <- function(dataMatrix, filename, cumulativeSws2, runcount){
  plotName = paste("Sws2Plot_", unlist(str_split(filename,"_"))[1], ".png", sep="") 
  png(plotName)
  
  plot(dataMatrix$step, dataMatrix$Sws2, type="o")

  # Regression Line
  res=lm(dataMatrix$Sws2~dataMatrix$step)
  abline(res, col='red', lwd='3')

  dev.off()

  # Cumulative Data Generation
  cumulativeSws2[,runCount] <- dataMatrix$Sws2 # add data to cumulative dataFrame
  return(cumulativeSws2)
}

print("Debug13")
# Plots cumulative Sws2
PlotCumulativeSws2 <- function(cumulativeSws2){
  plotName = paste("CumulativeSws2Plot.png")
  png(plotName) # Make file

  # Plot Data
  plot(cumulativeSws2[,1], type='l', col="red") # Plots first line
  # loops through each run Data.
  for( i in seq(from=2, to=ncol(cumulativeSws2), by=1)){
    lines(cumulativeSws2[,i], col="red") # adds line for other runs
  }

  dev.off() # close file
}

#-------------------------------------------------------------------------------
#------------------------------- Execution Code --------------------------------
#-------------------------------------------------------------------------------

print("Debug14")
# Set currDir to directory the script is run from.
currDir = getwd()
setwd(currDir)

paramList = Sys.glob("BROKE_ModelRun*")
count = 1
print("Debug15")
# Loops through each folder containing runs of a different parameter set.
for(paramSet in paramList){
  print(paramSet)
  setwd(paramSet)
  runsList = Sys.glob("run*")

  # Generate Cumulative Data Frames
  dataMatrixSample      <- MakeDataMatrix(runsList[1])
  cumulativeSws         <- data.frame(c(1:nrow(dataMatrixSample)))
  cumulativePathLength  <- data.frame(c(1:nrow(dataMatrixSample)))
  cumulativeClustering  <- data.frame(c(1:nrow(dataMatrixSample)))
  cumulativeHubCount    <- data.frame(c(1:nrow(dataMatrixSample)))
  cumulativeCC          <- data.frame(c(1:nrow(dataMatrixSample)))
  cumulativeSws2        <- data.frame(c(1:nrow(dataMatrixSample)))

  
  # loops through the data file for each run.
  runCount = 1
  for( run in runsList){
    print("Ryan :) ")    
    print(run)
    
    dataMatrix            <- MakeDataMatrix(run)
    cumulativeSws         <- PlotSws(dataMatrix, run, cumulativeSws, runCount)
    cumulativePathLength  <- PlotPathLength(dataMatrix, run, cumulativePathLength, runCount)
    cumulativeClustering  <- PlotClustering(dataMatrix, run, cumulativeClustering, runCount)
    cumulativeHubCount    <- PlotHubCount(dataMatrix, run, cumulativeHubCount, runCount)
    cumulativeCC          <- PlotCC(dataMatrix, run, cumulativeCC, runCount)
    cumulativeSws2        <- PlotSws2(dataMatrix, run, cumulativeSws2, runCount)

    runCount = runCount + 1
  }
    # Generates Cumulative Plots of all runs in a parameter set.
#    PlotCumulativeSws(cumulativeSws)
#    PlotCumulativePathLength(cumulativePathLength)
#    PlotCumulativeClustering(cumulativeClustering)
#    PlotCumulativeHubCount(cumulativeHubCount)
#    PlotCumulativeCC(cumulativeCC)
#    PlotCumulativeSws2(cumulativeSws2)

    # Makes folders for each plot type and move them in.
    #Sws
    system("mkdir SwsPlots")
    system("mv SwsPlot* SwsPlots/")
    #Path Length
    system("mkdir PathLengthPlots")
    system("mv PathLengthPlot* PathLengthPlots/")
    #Clustering
    system("mkdir TransitivityPlots")
    system("mv TransitivityPlot* TransitivityPlots/")
    #HubCounts
    system("mkdir HubCountPlots")
    system("mv HubCountPlot* HubCountPlots/")
    #CC
    system("mkdir CCPlots")
    system("mv CCPlot* CCPlots/")
    #Sws2
    system("mkdir Sws2Plots")
    system("mv Sws2Plot* Sws2Plots/")


  setwd("..")  # Move up a directory
}
print("ugh2")
