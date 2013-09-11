#!/bin/bash

runCount=20

for i in $(eval echo {1..$runCount})
do
    mkdir model$i
done

Rscript make_model.r