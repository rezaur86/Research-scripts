import sys, gc
import array
import operator
from sets import Set
from random import choice
import numpy as np

    
CLR_THRESHOLD = 500000
f = open(sys.argv[1], "r")
lifespan_bin_size = int(sys.argv[3])
lifespan_indeg_stat = {}
lifespan_odeg_stat = {}
act_lifespan_indeg_stat = {}
act_lifespan_odeg_stat = {}
count = 0
for line in f:
    element = line.split(',')
    node_id = int(element[0].strip())
    odeg = int(element[1].strip())
    indeg = int(element[2].strip())
    indeg_until_active = int(element[3].strip())
    act_lifespan = int(element[4].strip())
    lifespan = int(element[5].strip())
    sent_ARs = int(element[6].strip())
    count = count+1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    lifespan_bin = lifespan/lifespan_bin_size
    if(lifespan_bin in lifespan_indeg_stat):
        lifespan_indeg_stat[lifespan_bin].append(indeg)
        lifespan_odeg_stat[lifespan_bin].append(odeg)
    else:
        lifespan_indeg_stat[lifespan_bin] = [indeg]
        lifespan_odeg_stat[lifespan_bin] = [odeg]
    if odeg == 0:
        continue
    act_lifespan_bin = act_lifespan/lifespan_bin_size
    if(act_lifespan_bin in act_lifespan_odeg_stat):
        act_lifespan_odeg_stat[act_lifespan_bin].append(odeg)
        act_lifespan_indeg_stat[act_lifespan_bin].append(indeg)
    else:
        act_lifespan_odeg_stat[act_lifespan_bin] = [odeg]
        act_lifespan_indeg_stat[act_lifespan_bin] = [indeg]
        
f.close()

lifespan_stat_file = open(sys.argv[2]+"lifespan_stat.csv", "w")
for time in lifespan_indeg_stat:
    lifespan_stat_file.write('%s,%s,%s,%s\n'%(time, len(lifespan_indeg_stat[time]),
                                           round(np.average(lifespan_indeg_stat[time]), 3),
                                           round(np.average(lifespan_odeg_stat[time]),3)))
lifespan_stat_file.close()

act_lifespan_stat_file = open(sys.argv[2]+"act_lifespan_stat.csv", "w")
for time in act_lifespan_odeg_stat:
    act_lifespan_stat_file.write('%s,%s,%s,%s\n'%(time, len(act_lifespan_odeg_stat[time]),
                                            round(np.average(act_lifespan_indeg_stat[time]), 3),
                                            round(np.average(act_lifespan_odeg_stat[time]), 3)))
act_lifespan_stat_file.close()
