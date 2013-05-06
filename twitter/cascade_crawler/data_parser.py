import sys
import json
import urlparse
import httplib
import csv
import operator
import dateutil.parser, time, calendar


# Recursively follow redirects until there isn't a location header
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

def extract_tweets(data):
    global cascade_urls, cascade_id, user_id_index, user_ids, retweets_to_crawl, retweet_info
    if "statuses" in data:
        for each_status in data["statuses"]:
            resolved_urls = []
            if "urls" in each_status["entities"]:
                for each_url in each_status["entities"]["urls"]:
                    try:
                        resolved_url = resolve_http_redirect(each_url["url"])
                    except Exception as e:
                        print "Exception from resolve_http_redirect: %s\n" %e.args[0]
                        continue
                    resolved_urls.append(resolved_url)
                    if resolved_url not in cascade_urls:
                        cascade_urls[resolved_url] = cascade_id
                        cascade_id += 1
                if len(resolved_urls) < 1:
                    continue
            else:
                continue
            status_id = each_status["id"]
            status_created_time = calendar.timegm((dateutil.parser.parse(each_status["created_at"])).timetuple())
            retweet_count = each_status["retweet_count"]
            status_user_id = each_status["user"]["id"]
            if status_user_id not in user_ids:
                user_ids[status_user_id] = user_id_index
                user_id_index += 1
            if retweet_count > 0 :
                if status_id not in retweets_to_crawl:
                    retweets_to_crawl[status_id] = retweet_count
            if "retweeted_status" in each_status: # Then this is a retweet not a status
                retweeter_id = status_user_id
                retweeted_status_id = each_status["retweeted_status"]["id"]
                retweeted_status_user_id = each_status["retweeted_status"]["user"]["id"]
                if retweeted_status_user_id not in user_ids:
                    user_ids[retweeted_status_user_id] = user_id_index
                    user_id_index += 1
                retweeted_status_retweet_count = each_status["retweeted_status"]["retweet_count"]
                if retweeted_status_retweet_count > 0:
                    if retweeted_status_id not in retweets_to_crawl:
                        retweets_to_crawl[retweeted_status_id] = retweeted_status_retweet_count - 1
                    else:
                        retweets_to_crawl[retweeted_status_id] = retweets_to_crawl[retweeted_status_id] - 1
                retweet_time = status_created_time
                if retweets_to_crawl[retweeted_status_id] < 1:
                    del retweets_to_crawl[retweeted_status_id]
                for each_url in resolved_urls:
                    if (user_ids[retweeted_status_user_id], user_ids[retweeter_id], cascade_urls[each_url]) not in retweet_info:
                        retweet_info[(user_ids[retweeted_status_user_id], user_ids[retweeter_id], cascade_urls[each_url])] = retweet_time

cascade_urls = {}
cascade_url_and_status_id_map = []
cascade_id = 0
user_id_index = 0
user_ids = {}
retweets_to_crawl = {}
retweet_info = {}

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

writer = csv.writer(open('retweets_to_crawl.csv', 'wb'))
temp = sorted(retweets_to_crawl.iteritems(), key=operator.itemgetter(1), reverse=True)
writer.writerows(temp)

writer = csv.writer(open('cascade_urls.csv', 'wb'))
for key, value in cascade_urls.items():
    writer.writerow([key, value])

writer = csv.writer(open('user_ids.csv', 'wb'))
for key, value in user_ids.items():
    writer.writerow([key, value])

writer = csv.writer(open('retweet_info.csv', 'wb'))
for key, value in retweet_info.items():
    writer.writerow(key+(value,))
