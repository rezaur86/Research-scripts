#!/usr/bin/python
import os
import re
import signal
import json
from time import sleep, time
from threading import Thread, Lock


def update_queue(id):
    if id not in seen_set:
        seen_set.add(id)
        queue_lock.acquire()
        crawl_queue.append(id)
        queue_lock.release()

def save_output(data, output_file):       
    if data:
        output_file.write(str(data)+"\n") 

def parse_likes(cmd, output_file):
    result = parse_result(cmd)    
    plikes = "{\"ep_likes\" : " + str(result) + "}"
    save_output(plikes, output_file)            
    while True:
        try:    
            items = json.loads(result)
            if 'paging' in items and 'next' in items['paging']:
                sleep(10)
                cmd = "curl -s " + "\'" + str(items['paging']['next']) + "\'"                
                result = parse_result(cmd)
                plikes = "{\"ep_likes\" : " + str(result) + "}"
                save_output(plikes, output_file)                            
                                            
            else:                
                break                
                        
        except:
            break
        
    

def parse_comments(cmd, output_file): 
    result = parse_result(cmd)        
    pcomments = "{\"ec_comments\" : " + str(result) + "}"
    save_output(pcomments, output_file)        
    while True:
        try:    
            items = json.loads(result)            
            if 'data' in items:
                for data in items['data']:                    
                    if 'likes' in data:                        
                        sleep(10)
                        cmd = "curl -s " + posturl + str(data['id']) + "/likes"            
                        result = parse_result(cmd)
                        clikes = "{\"ec_likes\" : " + str(result) + "}"                
                        save_output(clikes, output_file)
                        while True:
                            try:    
                                items_c = json.loads(result)
                                if 'paging' in items_c and 'next' in items_c['paging']:
                                    sleep(10)
                                    cmd = "curl -s " + "\'" + str(items_c['paging']['next']) + "\'"                
                                    result = parse_result(cmd)
                                    clikes = "{\"ec_likes\" : " + str(result) + "}"
                                    save_output(clikes, output_file)                            
                                            
                                else:                    
                                    break                
                        
                            except:                                
                                break                    
                            
                    
            if 'paging' in items and 'next' in items['paging']:
                sleep(10)
                cmd = "curl -s " + "\'" + str(items['paging']['next']) + "\'"                
                result = parse_result(cmd)
                pcomments = "{\"ec_comments\" : " + str(result) + "}"                
                save_output(pcomments, output_file)                        
                                            
            else:
                break                
                        
        except:
            print result
            break

def parse_result(cmd):
    try:    
        line = os.popen(cmd,"r") 
        result = line.readline()
        debug1 = re.findall(pattern1,result)
        debug2 = re.findall(pattern1,result)
        if debug1 or debug2:        
            sleep(600)        
            line = os.popen(cmd,"r")
            result = line.readline() 
    except: 
        result = "Curl Error on : " + str(cmd)                    
               
    return result
            
        # Needs a lock to avoid write/read hazard
        # seen_lock.acquire()
        # update_queue(friend)
        # seen_lock.release()
    
def worker():
    global seed_count
    while True:
        queue_lock.acquire()
        if len(crawl_queue) == 0:
            queue_lock.release()
            break
        id = crawl_queue.pop(0)
        queue_lock.release()
    print seed_count[id], id    
    #Crawls User Status.              
    cmd_post = "curl -s " + posturl + str(id)
    cmd_plikes = "curl -s " + posturl + str(id) + "/likes"
    cmd_comments = "curl -s " + posturl + str(id) + "/comments"    
    postData = parse_result(cmd_post)
    #status_lock.acquire()
    out_name = "outputs/" + str(seed_count[id]) + "_" + str(id) + "_wh.txt"
    outfile = open(out_name,"w")    
    save_output(postData, outfile)
    sleep(10)
    parse_likes(cmd_plikes, outfile)
    sleep(10)
    parse_comments(cmd_comments, outfile)    
        #status_lock.release()
    sleep(10)    
    
    
        


# We use an Lock to protect the seen list.
seen_lock = Lock()
seen_set = set()

pattern1 = re.compile(r"Calls to stream have exceeded the rate of 600 calls per 600 seconds")
pattern2 = re.compile(r"Application request limit reached")
posturl = "https://graph.facebook.com/"    


# The queue for crawler to fetch user id.
queue_lock = Lock()
crawl_queue = []

# Fill the queue with initial seeds.
seeds = open("pid001_2.txt", "r")
seed_count = {}
for line in seeds:
    line = line.replace("\n","")    
    stream = line.split("\t")    
    seed_count[stream[1]] = stream[2]
    crawl_queue.append(stream[1])
    seen_set.add(stream[1])
seeds.close()


# Needs another lock to protect output files.
status_lock = Lock()
#status = file("wh_posts.txt", "w")


worker_list = []
# Makes 10 workers, each of them will issue a request every 10 seconds.
# Total request rate: 10/10*3600 = 3600.
num_worker_threads = 10
signal.signal(signal.SIGPIPE, signal.SIG_IGN)
for i in range(num_worker_threads):
    worker_id = Thread(target = worker)
    worker_list.append(worker_id)
    worker_id.daemon = True
    worker_id.start()

# Blocks until all tasks are done.
for worker_id in worker_list:
    worker_id.join()

#status.close()

