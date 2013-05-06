#!/usr/bin/python
import os
import sys
import re
import signal
from time import sleep, time
from threading import Thread, Lock
import json

def parse_result(cmd):
    try:    
        line = os.popen(cmd,"r")  
    except: 
        result = "Twurl Error on : " + str(cmd)
    else:   
        result = line.readline()
        debug = re.findall(pattern,result)
        if debug:        
            print debug
            sleep(60)
            line = os.popen(cmd,"r")
            result = line.readline()
        if result:
            crawled_data_file.write(result)
            crawled_data_file.write('\n')
            try:
                onedata = json.loads(result)
            except Exception as e:
                print "*********Json Loading************Error %s\n" % e.args[0] 
                return -1
            return onedata
            
pattern = re.compile(r"Rate limit exceeded")
url_status_retweets = "/1.1/statuses/retweets/"

status_ids_file = open(sys.argv[1], "r")
status_count = 0
crawled_data_file = open('crawl_data_' + sys.argv[1] + '_' + str(status_count) + '.txt', "a")
for line in status_ids_file:
    splits = line.split(',')
    status_id = splits[0].strip()
    retweet_count = int(splits[1].strip())
    cmd = "twurl \'" + url_status_retweets + status_id + ".json?count=100\'"
    pageData = parse_result(cmd)
    sleep(60)
    status_count += 1
    print status_count
    if (status_count % 60) == 0: #every hour save and close 60 status in a file
        crawled_data_file.close()
        crawled_data_file = open('crawl_data_' + sys.argv[1] + '_' + str(status_count) + '.txt', "a")
crawled_data_file.close()
