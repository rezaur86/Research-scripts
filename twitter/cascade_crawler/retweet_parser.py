import sys
import json
import urlparse
import httplib
import csv
import operator
import dateutil.parser, time, calendar

def register_user(id):
    global user_id_index, user_ids
    if id not in user_ids:
        user_ids[id] = user_id_index
        user_id_index += 1
    return user_ids[id]

def extract_retweets(each_status):
    global cascade_urls, cascade_id, url_map, retweet_info
    if "retweeted_status" not in each_status: # Then this is a not retweet so continue
        return
    else:
        if "user" not in each_status["retweeted_status"]:
            return
        retweeted_status_user_id = register_user(each_status["retweeted_status"]["user"]["id"])
        tweet_created_time = calendar.timegm((dateutil.parser.parse(each_status["retweeted_status"]["created_at"])).timetuple())
        retweeter_user_id = register_user(each_status["id"])
        retweet_created_time = calendar.timegm((dateutil.parser.parse(each_status["created_at"])).timetuple())
        resolved_urls = []
        if "urls" in each_status["retweeted_status"]["entities"]:
            for each_url in each_status["retweeted_status"]["entities"]["urls"]:
                if each_url["url"] not in url_map:
                    print 'missing url from the map '+each_url["url"]
                    continue
                resolved_url = str(url_map[each_url["url"]])
                if resolved_url == None:
                    continue
                resolved_urls.append(resolved_url)
                if resolved_url not in cascade_urls:
                    cascade_urls[resolved_url] = cascade_id
                    cascade_id += 1
            if len(resolved_urls) < 1:
                return
        else:
            return
        for each_url in resolved_urls:
            if (retweeted_status_user_id, retweeter_user_id, cascade_urls[each_url]) not in retweet_info:
                retweet_info[(retweeted_status_user_id, retweeter_user_id, cascade_urls[each_url])] = retweet_created_time
            if (0, retweeted_status_user_id, cascade_urls[each_url]) not in retweet_info:
                retweet_info[(0, retweeted_status_user_id, cascade_urls[each_url])] = tweet_created_time

cascade_urls = {}
cascade_id = 0
user_id_index = 1
user_ids = {}
retweet_info = {}
url_map = {}
for key, val in csv.reader(open('url_map_2702.csv', "r")):
    url_map[key] = val

keywords_file = open(sys.argv[1],"r")
line_index = 1
for a_keyword in keywords_file: #Extract retweets from the search data
    data_file = open(a_keyword.strip()+'.txt',"r")
    print a_keyword
    for each_json_line in data_file:
#        print line_index
        line_index += 1
        try:
            onedata = json.loads(each_json_line)
        except Exception as e:
            print "*********Json Loading************Error %s\n" % e.args[0]
            continue
        if "statuses" in onedata:
            for each_status in onedata["statuses"]:
                extract_retweets(each_status)
    data_file.close()   
retweet_status_file_names = sys.argv[2].split(',')
for a_retweet_status_file in retweet_status_file_names: #Extract retweets from the retweet crawled data
    data_file = open(a_retweet_status_file.strip(),"r")
    for each_json_line in data_file:
#        print line_index
        line_index += 1
        try:
            onedata = json.loads(each_json_line)
        except Exception as e:
            print "*********Json Loading************Error %s\n" % e.args[0]
            continue
        for each_retweet_status in onedata:
            extract_retweets(each_retweet_status)
    data_file.close()

print "Number of Cascades: %s"%cascade_id
print "Number of Users: %s"%user_id_index
writer = csv.writer(open('retweet_info.csv', 'wb'))
for key, value in retweet_info.items():
    writer.writerow(key+(value,))
