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
            json_result["post_id"] = record[0]
            json_result["link"] = record[1]
            json_result["freq"] = record[2] if record[2] is not None else 0
            json_result["entropy"] = str(record[3]) if record[3] is not None else 0
            json_result["shares"] = record[4] if record[4] is not None else 0
            json_result["likes"] = record[5]if record[5] is not None else 0
            json_result["comments"] = record[6] if record[6] is not None else 0
            json_result["createdtime"] = str(record[7])
            json_result["group"] = str(record[8])
            json_output["results"].append(json_result)
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print "*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror)
        return -1

form = cgi.FieldStorage()
results = []
json_output = {}
terms = ""

try:
    if form.has_key("terms"):
        terms = form.getvalue("terms")
        terms_type = 'AND' if form.getvalue("search_type") == 'ALL' else 'OR'
        json_output["method"] = terms_type
        words = terms.split()
        json_output["words"] = '|'.join(words)
        query = ''' select  * from (select distinct on (post_row_id) post_row_id, address, freq, entropy, shares_count, likes_count, comments_count, created_time, group_name from search as s JOIN (select row_id from keyword where'''
        words_count = len(words)
        print 
        for each_word in words:
            query += ''' word ~* '%s' '''%each_word
            words_count -= 1
            if words_count > 0:
                query += ' %s '%terms_type
            else:
                if form.getvalue("ranking") == 'Entropy':
                    query += ') as t ON s.keyword_row_id = t.row_id) as temp order by entropy desc,freq desc limit '
                elif form.getvalue("ranking") == 'Frequency':
                    query += ') as t ON s.keyword_row_id = t.row_id) as temp order by freq desc limit '
                elif form.getvalue("ranking") == 'Shares':
                    query += ') as t ON s.keyword_row_id = t.row_id) as temp where shares_count is not Null order by shares_count desc,freq desc limit '
                elif form.getvalue("ranking") == 'Likes':
                    query += ') as t ON s.keyword_row_id = t.row_id) as temp where likes_count is not Null order by likes_count desc,freq desc limit '
                elif form.getvalue("ranking") == 'Comments':
                    query += ') as t ON s.keyword_row_id = t.row_id) as temp where comments_count is not Null order by comments_count desc,freq desc limit '
        
        query += str(form.getvalue("total_results"))                       
        results = search (query)
        print json.dumps(json_output)
    else:
        terms = "UC Davis"
except NameError:
    print "There was an error understanding your search request.  Please press the back button and try again."
except:
    print "Really Unexpected error:", sys.exc_info()[0]
