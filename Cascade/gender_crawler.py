#!/usr/bin/python
import os,sys
import csv
import re
import signal
import json
import time
import threading
import operator
import array
import pycurl
import cStringIO

THREAD_COUNT = 10
BATCH_SIZE = 1000
graph_url = "https://graph.facebook.com/"
error1 = re.compile(r"Calls to stream have exceeded the rate of 600 calls per 600 seconds")
error2 = re.compile(r"Application request limit reached")
error3 = re.compile(r"Unknown fields")

SHRINK_BATCH_SIZE = 0
GENDER_ABSENT = 1
SERVER_ERROR = 2

def extract_gender(data):
    batch_genders = []
    try:
        onedata = json.loads(data)
        for an_id in onedata.keys():
            if onedata[an_id].has_key("gender"):
                batch_genders.append((an_id,onedata[an_id]["gender"]))
    except Exception, e:
        print "Gender Extracting, Json Error: %s"%e
    return batch_genders

def curl_request (url):
    response = cStringIO.StringIO()
    c = pycurl.Curl()
    c.setopt(c.URL, url)
    c.setopt(c.WRITEFUNCTION, response.write)
    c.setopt(c.NOSIGNAL, 1)
    c.perform()
    c.close()
    return response.getvalue()
    
def batch_crawler (batch_ids, batch_size, repeat_limit):
    if repeat_limit < 0:
        return SERVER_ERROR
    if len(batch_ids) > 0:
        batch_url = graph_url + "?fields=gender&ids=" +  ','.join(batch_ids)
    else:
        return None
#    print batch_url
    try:
        time.sleep(10)
        gender_json = ''.join((curl_request(batch_url)).split('\n'))
        rerun1 = re.findall(error1,gender_json)
        rerun2 = re.findall(error2,gender_json)
        if rerun1 or rerun2:
            print "Going to sleep for 600sec:", rerun1,rerun2        
            time.sleep(600)
            gender_json = ''.join(curl_request(batch_url).split('\n'))
        rerun3 = re.findall(error3,gender_json)
        extracted_genders = extract_gender(gender_json)
        if rerun3 or len(extracted_genders) == 0:
            if batch_size > 1:
                return SHRINK_BATCH_SIZE
            else:
                return GENDER_ABSENT
        else:
            return extracted_genders
    except Exception, e:
        print "Server Error: %s, Repeating now"%e
        return batch_crawler (batch_url, batch_size, repeat_limit-1)
    

class CrawlerThread (threading.Thread):
    def __init__(self, threadID, name):
        super(CrawlerThread, self).__init__()
        self.threadID = threadID
        self.name = name
    def run(self):
#        print "Starting " + self.name
        UserInfoWorker()
#        print "Exiting " + self.name

def UserInfoWorker():
    global output
    global gender_absent_ids
    while True:
        batch = 0
        batch_ids = []
        batch_genders = []
        with user_id_pop_lock:
            if len(user_ids) > 0:
                an_id = user_ids.pop()
                batch_ids.append(str(an_id))
                batch += 1
            else:
                break
        while batch < BATCH_SIZE:
            with user_id_pop_lock:
                if len(user_ids) > 0:
                    an_id = user_ids.pop()
                    batch_ids.append(str(an_id))
                    batch += 1
                else:
                    break
        extracted_genders = batch_crawler(batch_ids[0:BATCH_SIZE], BATCH_SIZE, 2)
        if extracted_genders == SHRINK_BATCH_SIZE:
            for i in range(10):
                extracted_genders_i = batch_crawler(batch_ids[100*i:100*(i+1)], 100, 2)
                if extracted_genders_i == SHRINK_BATCH_SIZE:
                    for j in range(10):
                        extracted_genders_j = batch_crawler(batch_ids[100*i+10*j:100*i+10*(j+1)], 10, 2)
                        if extracted_genders_j == SHRINK_BATCH_SIZE:
                            for k in range(10):
                                extracted_genders_k = batch_crawler(batch_ids[100*i+10*j+1*k:100*i+10*j+1*(k+1)], 1, 2)
                                if extracted_genders_k == GENDER_ABSENT:
                                    with gender_absent_lock:
                                        gender_absent_ids.append(batch_ids[100*i+10*j+1*k:100*i+10*j+1*(k+1)])
                                elif type(extracted_genders_k) is list:
                                    batch_genders.extend(extracted_genders_k)
                        elif type(extracted_genders_j) is list:
                            batch_genders.extend(extracted_genders_j)
                elif type(extracted_genders_i) is list:
                    batch_genders.extend(extracted_genders_i)
        elif type(extracted_genders) is list:
            batch_genders.extend(extracted_genders)
        with output_lock:
            output.extend(batch_genders)

for each_entry in os.listdir(sys.argv[1]):
    print each_entry
    user_file = open(sys.argv[1]+'/'+each_entry, "r")
    user_ids = []
    user_id_pop_lock = threading.RLock()
    for line in user_file:
        line = line.replace("\n","")    
        user_ids.append(long(line.strip()))    
    user_file.close()
    
    total_users = len(user_ids)
#    print "Total user %s"%total_users
    # Needs another lock to protect output
    output = []
    output_lock = threading.RLock()
    gender_absent_ids = []
    gender_absent_lock = threading.RLock()
    
    thread_ids = []
    for i in range(THREAD_COUNT):
        thread = CrawlerThread(i, "Thread-"+str(i))
        thread_ids.append(thread)
        thread.start()    # Start new Threads
    
    while True:
        if len(user_ids) > 0:
            print len(user_ids)*100/total_users,'%'
            time.sleep(20)
            if len(output) > 10000:
                with output_lock:
                    o_gender_json = open (sys.argv[2]+'/'+each_entry, "a")
                    writer = csv.writer(o_gender_json, quoting=csv.QUOTE_MINIMAL)
                    writer.writerows(output)
                    o_gender_json.close()
                    output = []
                    
        else:
            break
    for a_thread in thread_ids:
        a_thread.join()
    
    if len(output) > 0:
        with output_lock:
            o_gender_json = open (sys.argv[2]+'/'+each_entry, "a")
            writer = csv.writer(o_gender_json, quoting=csv.QUOTE_MINIMAL)
            writer.writerows(output)
            o_gender_json.close()
    if len(sys.argv) > 3:
        o_absent_ids = open (sys.argv[3]+'/'+each_entry, "w")
        for an_id in gender_absent_ids:
            o_absent_ids.write("%s\n" %an_id)
        o_absent_ids.close()
    
#    print "Exiting Main Thread"
