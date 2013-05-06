import sys
import json
import urlparse
import httplib
import csv
import operator
import dateutil.parser, time, calendar

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

def get_urls(each_status):
    global url_mapping
    url_count = 0
    if "urls" not in each_status["entities"]:
        return url_count
    else: #check if at least one url exists
        for each_url in each_status["entities"]["urls"]:
            if "url" not in each_url:
                continue
            else:
                if each_url["url"] not in url_mapping:
                    url_mapping[each_url["url"]] = None
                    try:
                        resolved_url = resolve_http_redirect(each_url["url"])
                    except Exception as e:
                        print "Exception from resolve_http_redirect: %s\n" %e.args[0]
                        continue
                    url_mapping[each_url["url"]] = resolved_url
                url_count += 1   
    return url_count
    
url_mapping = {}
for key, val in csv.reader(open('url_map_2100.csv', "r")):
    url_mapping[key] = val
    
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
                    url_count = get_urls(each_status["retweeted_status"])
                else:
                    url_count = get_urls(each_status)
        if (line_index % 100) == 0:
            writer = csv.writer(open('url_map_'+str(line_index)+'.csv', 'wb'))
            for key, value in url_mapping.items():
                writer.writerow([key, value])
        line_index += 1

writer = csv.writer(open('url_map_'+str(line_index)+'.csv', 'wb'))
for key, value in url_mapping.items():
    writer.writerow([key, value])
