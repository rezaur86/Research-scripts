#!/usr/bin/python
import os
import sys
import re
import signal
from time import sleep, time
from threading import Thread, Lock
import json

def read_next_result_page(data):
    if "search_metadata" in data:
        if "next_results" in data["search_metadata"]:
            return data["search_metadata"]["next_results"]
        else:
            return None
    else:
        return None

def parse_result(cmd):
    try:    
        line = os.popen(cmd,"r")  
    except: 
        result = "Twurl Error on : " + str(cmd)
    else:   
        result = line.readline()
        debug = re.findall(pattern,result)
        if debug:        
            sleep(5)
            #print debug
            line = os.popen(cmd,"r")
            result = line.readline()
        if result:
            search_result_file.write(result)
            search_result_file.write('\n')
            try:
                onedata = json.loads(result)
            except Exception as e:
                print "*********Json Loading************Error %s\n" % e.args[0] 
                return -1
            return onedata
            
pattern = re.compile(r"Rate limit exceeded")
url_search = "/1.1/search/tweets.json"

keywords_file = open(sys.argv[1], "r")
for line in keywords_file:
    next_page = None
    page_number = 0
    query_str = "?q=" + line.strip() + "%20filter%3Alinks"
    search_result_file = open(line.strip()+'.txt', "a")
    while True:
        if page_number == 0:
            cmd = "twurl \'" + url_search + query_str + "&include_entities=true&count=100\'"
            pageData = parse_result(cmd)
            next_page = read_next_result_page(pageData)
        else:
            cmd = "twurl \'"  + url_search + next_page +"\'"
            sleep(5)
            pageData = parse_result(cmd)
            next_page = read_next_result_page(pageData)
        if next_page == None:
            break
        page_number += 1
        print page_number
        print cmd
    search_result_file.close()
