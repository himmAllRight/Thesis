#!/bin/bash

runCount=20

for i in $(eval echo {1..$runCount})
do
    mkdir model$i
    Rscript r_testing.r model$i
    # run make_model.r with model$i as the input arguement, that causes the script to run in that directory.
done


