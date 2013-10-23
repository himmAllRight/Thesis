********************************************************************************
********************************* Scripts List ********************************* 
********************************************************************************

run_model.py 
------------
Runs the models and allows for the looping and incrementing input parameters.
Simply a Python script that calls the R scripts in an organized manor and
generates the directory structure for storing the data in.

  input args: 
    [1] modelNum
      - The number corresponding to the type of model to run. Model numbers are a
        follows:
          1. Random (rand_make_model.r)
          2. Hubs1 Algorithm (hubs1_make_model.r)
          3. Hubs2 Algorithm (hubs2_make_model.r)
          4. PathLength (pathLength_make_model.r)
    [2] folderBase
      - The title of the data set. The top folder of the data directory will
        have this name with a timestamp at the end. Additionally, the sub
        folders will have this name with the graph parameters appended.

exec: python run_model.py modelNum folderBase



rand_make_model.r
-----------------
Generates a an small-world graph and runs a random algorithm on it to rancomly
move edges between nodes. It randomly selects two nodes (x and y) with a
connection between them and remove it. It then finds a node(z) not connected to 
x and creates an edge betwen x and z. It does this for the number of specified
time stamps and records graph attributes such as average pathlength, SWS,
hub count, and clustering coefficient at each step.

  input args: (mostly parameters for graph generation)
    [1] topFolder
          - The same description as the folderBase above. It is the same thing
    [2] name
          - Name of run
    [3] dimension
          - The parameter for the dimension of the starting lattice when
            generating the graphs.
    [4] size
          - The parameter for the size of the starting lattice when generating
            the graphs.
    [5] nei
          - The neighborhood parameter for the starting lattice when generating
            the graphs.
    [6] p
          - The rewireing probability constant when generating the SWP graph.
    [7] trialCount
          - The number of times to run the model (number of seeds)
    [8] timeSteps
          - The number of time steps to run each model.

exec: Rscript rand_make_model.r topFolder name dimension size nei p trialCount
                                                                    timeSteps

*NOTE: This will usually be run from the run_model.py script and will be taken
       care of in that script.



hubs1_make_model.r
------------------

