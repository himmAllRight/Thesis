#!/usr/bin/python
import os

runCount = 0
# Loop through dimensions
for dim in range(1,6):

  # Loop through size
  for size in range(1,6):

    # Loop through nei
    for nei in range(1,6):

      # Loop through p
      p = 0.00
      for p_count in range(1,6):
        p += 0.1
        # Run Model
        folderName = 'randModelRun' + '_d' + str(dim) + '_s' + str(size) + '_n' + str(nei) + '_p' + str(p_count)
        cmd = 'Rscript make_model.r' + ' ' + folderName + ' ' + str(dim) + ' ' + str(size) + ' ' + str(nei) + ' ' + str(p) +' 5 1000'
        print(cmd)
        os.system(cmd)
        runCount += 1
