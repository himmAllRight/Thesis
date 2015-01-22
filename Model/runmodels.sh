#!/bin/bash

echo -e "Time Test, RAM disk - Start" `date` >> timeTest.txt
python3 run_model.py 2 1 >> Logs/timeTest_log.txt
python3 run_model.py 4 1 >> Logs/timeTest_log.txt
echo -e "Time Test, RAM disk - End" `date` >> timeTest.txt
