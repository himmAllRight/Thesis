#!/usr/bin/python
import os
import sys
import datetime

modelNum   = int(sys.argv[1])
folderBase = sys.argv[2]

# Used to make time Stamped folder
now       = datetime.datetime.now()
timeStamp = str(now.year) + str(now.day) + str(now.hour) + str(now.minute)
topFolder = folderBase + '_' + timeStamp

# Makes main directory to run all runs in.
makeTopFolder = 'mkdir ' + topFolder
os.system(makeTopFolder)

# Different types of the model
modelType = ''
if(modelNum == 1):
  modelType = 'rand_make_model.r'
if(modelNum == 2):
  modelType = 'hubs1_make_model.r'
if(modelNum == 3):
  modelType = 'hubs2_make_model.r'
if(modelNum == 4):
  modelType = 'pathLength_make_model.r'

# Parameter/ Parameter Arrays
dimArray  = [2,3]                 # Dimension Values
sizeArray = [23,8]                # Size Values
neiArray  = [1]                   # Neighbor values
pArray    = [.1,.15,.25,.35]      # re-wiring prob. values
runs      = 30
steps     = 1000

runCount = 0
# Loop through dimensions
for i in [0,1]:
  dim  = dimArray[i]
  size = sizeArray[i]
#  # Loop through size
#  for size in range(30,31,5):

    # Loop through nei
  for nei in neiArray:
    
    p_count = 1
    # Loop through p
    for p in pArray:
      p_count += 1
             
      # Run Model
      folderName = folderBase + '_d' + str(dim) + '_s' + str(size) + '_n' + str(nei) + '_p' + str(p_count)
      cmd = 'Rscript ' +  modelType + ' '+ topFolder + ' ' + folderName + ' ' + str(dim) + ' ' + str(size) + ' ' + str(nei) + ' ' + str(p) + ' ' + str(runs) + ' ' + str(steps)
      os.system(cmd)
        
      runCount += 1
