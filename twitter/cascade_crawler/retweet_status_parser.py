import sys
import json
import urlparse
import httplib
import csv
import operator
import dateutil.parser, time, calendar

def extract_tweets(data):
    global retweets_to_crawl
    if "statuses" in data:
        url_count = 0
        for each_status in data["statuses"]:
            if "urls" not in each_status["entities"]:
                continue
            else: #check if at least one url exists
                for each_url in each_status["entities"]["urls"]:
                    if "url" not in each_url:
                        continue
                    else:
                        url_count += 1
            if url_count < 1:
                continue
            
            if "retweeted_status" in each_status: # Then this is a retweet not a status
                retweeted_status_id = each_status["retweeted_status"]["id"]
                retweeted_status_retweet_count = each_status["retweeted_status"]["retweet_count"]
                if retweeted_status_retweet_count > 0:
                    if retweeted_status_id not in retweets_to_crawl:
                        retweets_to_crawl[retweeted_status_id] = retweeted_status_retweet_count
            else:
                status_id = each_status["id"]
                retweet_count = each_status["retweet_count"]
                if retweet_count > 0 :
                    if status_id not in retweets_to_crawl:
                        retweets_to_crawl[status_id] = retweet_count

 
retweets_to_crawl = {}

keywords_file = open(sys.argv[1],"r")
line_index = 1
for a_keyword in keywords_file:
    data_file = open(a_keyword.strip()+'.txt',"r")
    print a_keyword
    for each_json_line in data_file:
        print line_index
        line_index += 1
        try:
            onedata = json.loads(each_json_line)
        except Exception as e:
            print "*********Json Loading************Error %s\n" % e.args[0]
        extract_tweets(onedata)

writer = csv.writer(open('status_ids.csv', 'wb'))
temp = sorted(retweets_to_crawl.iteritems(), key=operator.itemgetter(1), reverse=True)
writer.writerows(temp)
