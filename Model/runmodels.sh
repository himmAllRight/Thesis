#!/bin/bash

echo $(date) >> Logs/March24_times.txt
python3 run_model.py 1 .85 >> Logs/March24_Run_log.txt
echo $(date) >> Logs/March24_times.txt
python3 run_model.py 2 .85 >> Logs/March24_Run_log.txt 
echo $(date) >> Logs/March24_times.txt
python3 run_model.py 1 .87 >> Logs/March24_Run_log.txt 
echo $(date) >> Logs/March24_times.txt
python3 run_model.py 2 .87 >> Logs/March24_Run_log.txt 
echo $(date) >> Logs/March24_times.txt
python3 run_model.py 1 .90 >> Logs/March24_Run_log.txt 
echo $(date) >> Logs/March24_times.txt
python3 run_model.py 2 .90 >> Logs/March24_Run_log.txt 
echo $(date) >> Logs/March24_times.txt
python3 run_model.py 1 .92 >> Logs/March24_Run_log.txt 
echo $(date) >> Logs/March24_times.txt
python3 run_model.py 2 .92 >> Logs/March24_Run_log.txt
echo $(date) >> Logs/March24_times.txt
