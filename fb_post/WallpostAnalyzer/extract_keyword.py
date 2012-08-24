import os, sys
import re
from topia.termextract import extract
import psycopg2
import csv
sys.path.append(os.path.abspath('../jsonParser/'))
from db_connection import openDb, closeDB


def extract_keyword(text):
    extractor = extract.TermExtractor()
    extractor.filter = extract.DefaultFilter(singleStrengthMinOccur=2, noLimitStrength=65535)
    keywords = sorted(extractor(text))
    return keywords

def read_story (group_id, by_post = False):
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        cursor.execute('select row_id, name from fb_user where id = %s', (group_id,))
        if cursor.rowcount > 0:
            group_info = cursor.fetchone()
            group_row_id = group_info[0]
            name = group_info[1]
        else:
            return None
        global keyword_post_row_id
        if by_post == False:
            story = ''
            cut_off = 100
            cursor.execute('''select string_agg(name||', '||description||', '||caption||' : '||text,E'\n') from message where fb_wall_row_id = %s''', (group_row_id,))
            if cursor.rowcount > 0:
                story = cursor.fetchone()[0]
            else:
                return None
        else:
            cut_off = 100
            cursor.execute('''select parent_message_row_id, string_agg(ARRAY_TO_STRING(ARRAY[name,description,caption,text], ', '),E'\n') from message where fb_wall_row_id = %s group by parent_message_row_id''', (group_row_id,))
            for record in cursor:
                story = record[1]
                story_keywords = extract_keyword(story)
                for each_keyword in story_keywords:
#                    keyword_phrase = re.split("[^{a-zA-Z0-9}\-!@#\$%&\*\+\?_]+", each_keyword[0]) #(" ".join(re.split("[^a-zA-Z]+", each_keyword[0])))
#                    for each_word in keyword_phrase:
                    keyword_row_id = register_keywords((each_keyword[0]).lower()) #(" ".join(re.split("[^{a-zA-Z0-9}\-!@#\$%&\*\+\?_]+", each_keyword[0]))).lower())
                    keyword_post_row_id += 1
                    new_keyword_post.append((keyword_post_row_id, keyword_row_id, record[0], each_keyword[1]))    
#                cut_off -= 1
#                if cut_off < 1:
#                    break     
        closeDB(conn, cursor)
        if (by_post == False):
            return story
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1

def register_keywords(a_keyword):
    global all_keywords
    global keyword_row_id
    if all_keywords.has_key(a_keyword):
        return all_keywords[a_keyword]
    else:
        keyword_row_id += 1
        new_keywords.append((keyword_row_id, a_keyword))
        all_keywords[a_keyword] = keyword_row_id
        return keyword_row_id
 
def init_extraction():
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        global keyword_row_id
        global all_keywords
        global keyword_post_row_id
        cursor.execute('select max(row_id) from keyword_temp')
        keyword_row_id = cursor.fetchone()[0]
        if keyword_row_id is None:
            keyword_row_id = 0
        cursor.execute('select row_id, word from keyword_temp')
        if cursor.rowcount > 0:
            for record in cursor:
                all_keywords[record[1]] = record[0]

        cursor.execute('select max(row_id) from keyword_post_temp')
        keyword_post_row_id = cursor.fetchone()[0]
        if keyword_post_row_id is None:
            keyword_post_row_id = 0
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1    

def push_new_data():
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        cursor.executemany('insert into keyword(row_id, word) values (%s, %s)', new_keywords)
        cursor.executemany('insert into keyword_post(row_id, keyword_row_id, post_row_id, freq) values (%s, %s, %s, %s)', new_keyword_post)
        closeDB(conn, cursor)
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        print ("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1    
    
groups = sys.argv[1].split(',')

new_keywords = []
all_keywords = {}
keyword_row_id = 0
keyword_post_row_id = 0
new_keyword_post = []
init_extraction()

for each_group in groups:
    read_story(each_group, True)
    print each_group,'done'

#push_new_data()

keyword_file =  open ("keyword.csv", 'w')
writer = csv.writer(keyword_file, quoting=csv.QUOTE_MINIMAL)
writer.writerows(new_keywords)
keyword_file.close()

keyword_post_file =  open ("keyword_post.csv", 'w')
writer = csv.writer(keyword_post_file, quoting=csv.QUOTE_MINIMAL)
writer.writerows(new_keyword_post)
keyword_post_file.close()
