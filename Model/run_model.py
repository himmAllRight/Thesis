#!/usr/bin/python
import os
import sys
import datetime

modelNum       = int(sys.argv[1])
modelTypeName  = 'Wrong_Type_Selected'
#folderBase = sys.argv[2]


# Used to make time Stamped folder
now       = datetime.datetime.now()
timeStamp = str(now.year) + str(now.month) +  str(now.day) + str(now.hour) + str(now.minute)
#topFolder = folderBase + '_' + timeStamp

# Different types of the model
modelType = ''
if(modelNum == 1):
  modelType     = 'rand_make_model.r'
  modelTypeName = 'rand_'
if(modelNum == 2):
  modelType     = 'hubs1_make_model.r'
  modelTypeName = 'hubs1_'
if(modelNum == 3):
  modelType     = 'hubs2_make_model.r'
  modelTypeName = 'hubs2_'
if(modelNum == 4):
  modelType     = 'hubs3_make_model.r'
  modelTypeName = 'hubs3_'
if(modelNum == 5):
  modelType     = 'pathLength_make_model.r'
  modelTypeName = 'pathlength_'
if(modelNum == 7):
  modelType     = 'hubs1_make_model_R.r'
  modelTypeName = 'hubs1_testing_'


# Makes folder Names
folderBase = 'ModelRun_' + modelTypeName
topFolder  = folderBase + timeStamp

# Makes main directory to run all runs in.
makeTopFolder = 'mkdir ' + topFolder
os.system(makeTopFolder)

# Parameter/ Parameter Arrays
dimArray  = [4]            # Dimension Values
sizeArray = [3]           # Size Values
neiArray  = [1,]              # Neighbor values
pArray    = [.25]  # re-wiring prob. values
threshold = float(sys.argv[2])                 # Hub Threshold value
runs      = 5                 # Number of seed Runs
steps     = 1000               # Number of steps per run

runCount = 0
# Loop through dimensions
for i in range(0,len(dimArray)):
  dim  = dimArray[i]
  size = sizeArray[i]
#  # Loop through size
#  for size in range(30,31,5):

    # Loop through nei
  for nei in neiArray:
    
    p_count = 0 
    # Loop through p
    for p in pArray:
      p_count += 1
             
      # Run Model
      folderName = folderBase + 'd' + str(dim) + '_s' + str(size) + '_n' + str(nei) + '_t' + str(int(threshold*100)) + '_p' + str(p_count)

      #cmd = 'Rscript ' + modelType  + ' ' + folderName + ' ' + str(dim) + ' ' + str(size) + ' ' + str(nei) + ' ' + str(p) + ' ' + str(threshold*10) + ' ' + str(runs) + ' ' + str(steps)
      cmd = 'Rscript ' + modelType  + ' ' + topFolder + ' ' + folderName + ' ' + str(dim) + ' ' + str(size) + ' ' + str(nei) + ' ' + str(p) + ' ' + str(threshold) + ' ' + str(runs) + ' ' + str(steps)

      print(topFolder)
      print(cmd)
      os.system(cmd)
        
      runCount += 1
