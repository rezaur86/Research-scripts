import sys, gc
import array
import numpy as np
import operator
import time
from sets import Set
from random import choice

CLR_THRESHOLD = 500000

def cum_sum(list):
    sum = 0
    cum_list = []
    for tuple in list:
        sum += tuple[1]
        cum_list.append((tuple[0], sum))
    return cum_list

births = {}
activations = {}
last_acts = {}
last_seens = {}
count = 0
f = open(sys.argv[1], "r")
for line in f:
    count += 1
    if (count % (CLR_THRESHOLD/10)) == 0:
        print count
    element = line.split(' ')
    node_id = int(element[0].strip())
    born_time = int(element[1].strip())
    activation_time = int(element[2].strip())
    last_act_time = int(element[3].strip())
    last_seen_time = int(element[4].strip())
    is_leaf = bool(int(element[5].strip()))
    odeg = int((element[6].strip()))
    parent_list = element[7:len(element)]

    tmp_time = time.localtime(born_time)
    born_day = (time.strftime("%Y", tmp_time),
                      time.strftime("%U", tmp_time),
                      time.strftime("%w", tmp_time),
                      time.strftime("%H", tmp_time))
    if born_day not in births:
        births[born_day] = 1
    else:
        births[born_day] += 1
    tmp_time = time.localtime(last_seen_time)
    last_seen_day = (time.strftime("%Y", tmp_time),
                      time.strftime("%U", tmp_time),
                      time.strftime("%w", tmp_time),
                      time.strftime("%H", tmp_time))
    if last_seen_day not in last_seens:
        last_seens[last_seen_day] = 1
    else:
        last_seens[last_seen_day] += 1
    if is_leaf == False:
        tmp_time = time.localtime(activation_time)
        activation_day = (time.strftime("%Y", tmp_time),
                          time.strftime("%U", tmp_time),
                          time.strftime("%w", tmp_time),
                          time.strftime("%H", tmp_time))
        if activation_day not in activations:
            activations[activation_day] = 1
        else:
            activations[activation_day] += 1
        tmp_time = time.localtime(last_act_time)
        last_act_day = (time.strftime("%Y", tmp_time),
                          time.strftime("%U", tmp_time),
                          time.strftime("%w", tmp_time),
                          time.strftime("%H", tmp_time))
        if last_act_day not in last_acts:
            last_acts[last_act_day] = 1
        else:
            last_acts[last_act_day] += 1
            
f.close()

# temp = sorted(activations.iteritems(), key=operator.itemgetter(0), reverse=False)

daily_born_file = open(sys.argv[2]+"daily_born.csv", "w")
for tuple in births:
    daily_born_file.write('%s,%s,%s,%s,%s\n'%(tuple[0],tuple[1],tuple[2],tuple[3],births[tuple]))
daily_born_file.close()

daily_activation_file = open(sys.argv[2]+"daily_activation.csv", "w")
for tuple in activations:
    daily_activation_file.write('%s,%s,%s,%s,%s\n'%(tuple[0],tuple[1],tuple[2],tuple[3],activations[tuple]))
daily_activation_file.close()

daily_last_act_file = open(sys.argv[2]+"daily_last_act.csv", "w")
for tuple in last_acts:
    daily_last_act_file.write('%s,%s,%s,%s,%s\n'%(tuple[0],tuple[1],tuple[2],tuple[3],last_acts[tuple]))
daily_last_act_file.close()

daily_last_seen_file = open(sys.argv[2]+"daily_last_seen.csv", "w")
for tuple in last_seens:
    daily_last_seen_file.write('%s,%s,%s,%s,%s\n'%(tuple[0],tuple[1],tuple[2],tuple[3],last_seens[tuple]))
daily_last_seen_file.close()
