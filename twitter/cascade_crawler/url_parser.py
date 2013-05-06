import sys
import json
import urlparse
import httplib
import csv
import operator
import dateutil.parser, time, calendar
import threading

def resolve_http_redirect(url, depth=0):
    if depth > 5:
        raise Exception("Redirected "+str(depth)+" times, giving up.")
    o = urlparse.urlparse(url,allow_fragments=True)
    conn = httplib.HTTPConnection(o.netloc)
    path = o.path
    if o.query:
        path +='?'+o.query
    conn.request("HEAD", path)
    res = conn.getresponse()
    headers = dict(res.getheaders())
    if headers.has_key('location') and headers['location'] != url:
        return resolve_http_redirect(headers['location'], depth+1)
    else:
        return url

class url_Thread (threading.Thread):
    def __init__(self, threadID, name):
        super(url_Thread, self).__init__()
        self.threadID = threadID
        self.name = name
    def run(self):
        print "Starting " + self.name
        url_worker()
        print "Exiting " + self.name

def url_worker ():
    while True:
        url_pool_lock.acquire()
        if len(url_pool) < 1:
            url_pool_lock.release()
            break
        each_url = url_pool.pop()
        url_pool_lock.release()
        try:
            resolved_url = resolve_http_redirect(each_url)
        except Exception as e:
            print "Exception from resolve_http_redirect: %s\n" %e.args[0]
            continue
        url_mapping_lock.acquire()
        url_mapping[each_url] = resolved_url
        url_mapping_lock.release()

def get_urls(each_status):
    global url_mapping
    if "urls" not in each_status["entities"]:
        return
    else: #check if at least one url exists
        for each_url in each_status["entities"]["urls"]:
            if "url" not in each_url:
                continue
            else:
                if each_url["url"] not in url_mapping:
                    url_mapping[each_url["url"]] = None
                    url_pool.append(each_url["url"])
    return

url_pool = []
url_pool_lock = threading.RLock()

url_mapping = {}
for key, val in csv.reader(open('url_map_2300.csv', "r")):
    url_mapping[key] = val
url_mapping_lock = threading.RLock()
    
keywords_file = open(sys.argv[1],"r")
line_index = 1
for a_keyword in keywords_file:
    data_file = open(a_keyword.strip()+'.txt',"r")
    print a_keyword
    for each_json_line in data_file:
        if line_index < 2101:
            line_index += 1
            continue
        print line_index
        try:
            onedata = json.loads(each_json_line)
        except Exception as e:
            print "*********Json Loading************Error %s\n" % e.args[0]
        if "statuses" in onedata:
            for each_status in onedata["statuses"]:
                if "retweeted_status" in each_status: # Then this is a retweet not a status
                    get_urls(each_status["retweeted_status"])
                else:
                    get_urls(each_status)
        line_index += 1

# Create threads
thread_count = 24
thread_ids = []
for i in range(thread_count):
    thread = url_Thread(i, "Thread-"+str(i))
    thread_ids.append(thread)
    thread.start()    # Start new Threads

save_interval = 0
while True:
    if len(url_pool) > 0:
        save_interval += 1
        if (save_interval%60) == 0:
            writer = csv.writer(open('url_map_'+str(len(url_pool))+'.csv', 'wb'))
            for key, value in url_mapping.items():
                writer.writerow([key, value])
        time.sleep(20)
        print len(url_pool)
    else:
        break
for a_thread in thread_ids:
    a_thread.join()

writer = csv.writer(open('url_map_'+str(len(url_pool))+'.csv', 'wb'))
for key, value in url_mapping.items():
    writer.writerow([key, value])
