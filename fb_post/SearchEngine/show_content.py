#!/usr/bin/env python
print "Content-Type: application/json"
print
import cgi
import sys,os
import json
from decimal import Decimal
import psycopg2
sys.path.append(os.path.abspath('../jsonParser/'))
from db_connection import openDb, closeDB
def search (query):
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        cursor.execute(query)
        global json_output
        json_output["results"] = []
        for record in cursor:
            json_result = {}
            json_result["row_id"] = record[0]
            json_result["createdtime"] = str(record[1])
            json_result["name"] = record[3] if record[3] is not None else ''
            json_result["text"] = record[4] if record[4] is not None else ''
            json_result["description"] = record[5] if record[5] is not None else ''
            json_result["caption"] = record[6] if record[6] is not None else ''
            json_result["likes"] = record[8]if record[8] is not None else 0
            
            json_output["results"].append(json_result)
            json_output['text'] = json_result["text"]
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print "*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror)
        return -1

form = cgi.FieldStorage()
results = []
json_output = {}
terms = ""

try:
    if form.has_key("post_id"):
        terms = form.getvalue("terms")
        post_id = form.getvalue("post_id")
        terms_type = '|' #if form.getvalue("search_type") == 'ALL' else '|'
        json_output["post_id"] = post_id
        json_output["method"] = terms_type
        words = terms.split('|')
        json_output["words"] = '|'.join(words)
        query = '''select row_id, created_time, fb_wall_row_id, name, text, description, caption, from_user_row_id, likes_count, comments_count, shares_count from message where parent_message_row_id = %s and ARRAY_TO_STRING(ARRAY[name,description,caption,text], ', ') ~* ''' %post_id
        words_count = len(words)
        query_terms = ''
        for each_word in words:
            query_terms += '(%s)'%each_word
            words_count -= 1
            if words_count > 0:
                query_terms += '%s'%terms_type
            else:
                query += ''' '%s' '''%query_terms
        results = search (query)
#        json_output['text'] = str(json_output['results'])
        print json.dumps(json_output)
    else:
        terms = "UC Davis"
except NameError:
    print "There was an error understanding your search request.  Please press the back button and try again."
except:
    print "Really Unexpected error:", sys.exc_info()[0]
