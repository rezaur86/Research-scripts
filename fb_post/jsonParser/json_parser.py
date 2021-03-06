import os
import re
import sys
import signal
import json
from time import sleep, time
from threading import Thread, Lock

fb_wall_last_value = 0
num_threads = 10
json_files = []
wall_id_of_files = {}
worker_list = []

csv = []
csv_counter = 0

if(len(sys.argv) > 1):
    for root, dirs, files in os.walk(sys.argv[1]):
        os.chdir(sys.argv[1])
        for dir in dirs:
            message_ids = {}
            fb_wall_last_value = fb_wall_last_value + 1
            for root_next, dirs_next, files_next in os.walk(dir):
                for each_file in files_next:
#                    print (sys.argv[1]+dir+'/'+each_file)
                    wall_id_of_files[sys.argv[1]+dir+'/'+each_file] = fb_wall_last_value
                    json_files.append(sys.argv[1]+dir+'/'+each_file)
                    #raw_input('press a key')
else:
    print('Give a path for Json directories')

def describe_this (json_strings):
    global csv_counter
    csv_lock.acquire()
    csv_counter = csv_counter + 1
    csv.append(csv_counter)
#    print json_strings + '\n'
    csv_lock.release()
    
def worker ():
    global json_files
    while True:
        json_files_lock.acquire()
        if len(json_files) == 0:
            json_files_lock.release()
            break
        json_file = json_files.pop(0)
        json_files_lock.release()
        json_strings = open (json_file)
#        print json_strings
        describe_this(json_strings)


json_files_lock = Lock()
csv_lock = Lock()
signal.signal(signal.SIGPIPE, signal.SIG_IGN)
for i in range(num_threads):
    worker_id = Thread(target = worker)
    worker_list.append(worker_id)
#    worker_id.daemon = True
    worker_id.start()

# Blocks until all tasks are done.
#for worker_id in worker_list:
#    worker_id.join()

#status.close()

