#!/usr/bin/python
import os
import sys
import datetime

folderBase = sys.argv[1]

# Used to make time Stamped folder
now       = datetime.datetime.now()
timeStamp = str(now.year) + str(now.day) + str(now.hour) + str(now.minute)
topFolder = folderBase + '_' + timeStamp

# Makes main directory to run all runs in.
makeTopFolder = 'mkdir ' + topFolder
os.system(makeTopFolder)

runCount = 0
# Loop through dimensions
for dim in range(2,3):

  # Loop through size
  for size in range(30,31):

    # Loop through nei
    for nei in range(1,2):

      # Loop through p
      p = 0.00
      for p_count in range(1,2):
        p += 0.1
        
        # Run Model
        folderName = 'randModelRun' + '_d' + str(dim) + '_s' + str(size) + '_n' + str(nei) + '_p' + str(p_count)
        cmd = 'Rscript make_model.r' + ' ' + ' '+ topFolder + ' ' + folderName + ' ' + str(dim) + ' ' + str(size) + ' ' + str(nei) + ' ' + str(p) +' 5 1000'
        os.system(cmd)
        
        runCount += 1
