import sys, os
import copy
from sets import Set
import csv
import array
import time
from bitarray import bitarray
import operator
import numpy as np

MAX_USERS = 2**29 - 1
NO_PARENT = MAX_USERS + 1
NEVER = 2**31 - 1

if __name__ == '__main__':
    gift_type_stat = {}
    activities = {}
    file_list = sorted(os.listdir(sys.argv[1]))
    user_last_seen_time = array.array('l')
    activity_line = 0
    for each_file in file_list:
        f = open(sys.argv[1]+'/'+each_file, "r")
        for line in f:
            activity_line += 1
            splits = line.split()
            sender = long(splits[0].strip())
            recv = long(splits[1].strip())
            timestamp = long(splits[2].strip())
            hid = int(splits[3].strip())
            if hid in gift_type_stat:
                gift_type_stat[hid] += 1
            else:
                gift_type_stat[hid] = 1
            tmp_time = time.localtime(timestamp)
            activity = (time.strftime("%Y", tmp_time),
                              time.strftime("%U", tmp_time),
                              time.strftime("%w", tmp_time),
                              time.strftime("%H", tmp_time))
            if activity not in activities:
                activities[activity] = 1
            else:
                activities[activity] += 1
        f.close()
        print each_file
    total_line = activity_line
    print 'Total line read: ', total_line
    #Gift type stat
    f = open(sys.argv[2]+'gift_type_stat.txt', "w")
    for each_gift in gift_type_stat:
        f.write('%s,%s\n' %(each_gift,gift_type_stat[each_gift]))
    f.close()
    #Activities
    f = open(sys.argv[2]+'activities_stat.txt', "w")
    for tuple in activities:
        f.write('%s,%s,%s,%s,%s\n'%(tuple[0],tuple[1],tuple[2],tuple[3],activities[tuple]))
    f.close()
