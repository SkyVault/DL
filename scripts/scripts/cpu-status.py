#!/usr/bin/python
import os
import sys
import re 
import time

def cpuUsage(stat, num_cpus, old_cpus):
    if old_cpus is None:
        old_cpus = [[0]*num_cpus]*num_cpus

    now_cpus = []
    for line in stat.splitlines(): 
        splits = []
        if line.startswith("cpu "): 
            splits = line.split(" ")[2:]
        else:
            splits = line.split(" ")[1:]

        now_cpus.append(splits) 

        if len(now_cpus) > num_cpus:
            break

    percents = [0] * num_cpus
    
    for i in range(num_cpus):
        idle_time = float(now_cpus[i][3]) - float(old_cpus[i][3])
        other_time = float(now_cpus[i][0]) - float(old_cpus[i][0]) + float(now_cpus[i][1]) - float(old_cpus[i][1]) + float(now_cpus[i][2]) - float(old_cpus[i][2])

        if idle_time + other_time != 0:
            percents[i] = (other_time*100)/(idle_time+other_time)            
        else:
            percents[i] = 0

    print(percents)

    return now_cpus, percents


old_cpus = None 
while True:

    contents = ""
    num_cpus = 0

    with open('/proc/stat', 'r') as f:
        contents = f.read() 
        num_cpus = len([m.start() for m in re.finditer('cpu', contents)]) - 1 

    old_cpus, percents = cpuUsage(contents, num_cpus, old_cpus)
    time.sleep(0.1)
    
