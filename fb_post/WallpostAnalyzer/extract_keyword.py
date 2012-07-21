import os, sys
from topia.termextract import extract
sys.path.append(os.path.abspath('../jsonParser/'))
from db_connection import openDb, closeDB

def extract_keyword(text):
    extractor = extract.TermExtractor()
    extractor.filter = extract.DefaultFilter(singleStrengthMinOccur=1, noLimitStrength=1)
    keywords = sorted(extractor(text))
    print keywords
    i = 0
    for each_keyword in keywords:
        i = i + 1
        print each_keyword
    print i 

def read_story (group_id, post_id = -1):
    try:
        (conn, cursor) = openDb(True, True)
    except psycopg2.Error, e:
        print "Error %d: %s" % (e.pgcode, e.pgerror)
        return -1
    try:
        cursor.execute('select row_id, id from fb_user')
        if cursor.rowcount > 0:
            for record in cursor:
                json_describer.fb_user_ids[record[1]] = record[0]
        else:
            return 0

        cursor.execute('select row_id, id from message')
        if cursor.rowcount > 0:
            for record in cursor:
                json_describer.message_ids[record[1]] = record[0]
        else:
            return 0

        cursor.execute('select max(row_id) from fb_user')
        json_describer.fb_user_last_value = cursor.fetchone()[0]
        
        cursor.execute('select max(row_id) from message')
        json_describer.message_last_value = cursor.fetchone()[0]
        
        cursor.execute('select max(row_id) from message_to')
        json_describer.message_to_last_value = cursor.fetchone()[0]

        cursor.execute('select max(row_id) from likedby')
        json_describer.likedby_last_value = cursor.fetchone()[0]
        
        cursor.execute('select max(row_id) from tag')
        json_describer.tag_last_value = cursor.fetchone()[0]

        cursor.execute('select max(row_id) from link')
        json_describer.link_last_value = cursor.fetchone()[0]

        closeDB(conn, cursor)
        return 1
    except psycopg2.Error, e:
        closeDB(conn, cursor)
        ERROR_FILE.write("*********Database************Error %s: %s\n" % (e.pgcode, e.pgerror))
        return -1

text = '''Police shut Palestinian theatre in Jerusalem.
...
... Israeli police have shut down a Palestinian theatre in East Jerusalem.
...
... The action, on Thursday, prevented the closing event of an international
... literature festival from taking place.
...
... Police said they were acting on a court order, issued after intelligence
... indicated that the Palestinian Authority was involved in the event.
...
... Israel has occupied East Jerusalem since 1967 and has annexed the
... area. This is not recognised by the international community.
...
... The British consul-general in Jerusalem , Richard Makepeace, was
... attending the event.
...
... "I think all lovers of literature would regard this as a very
... regrettable moment and regrettable decision," he added.
...
... Mr Makepeace said the festival's closing event would be reorganised to
... take place at the British Council in Jerusalem.
...
... The Israeli authorities often take action against events in East
... Jerusalem they see as connected to the Palestinian Authority.
...
... Saturday's opening event at the same theatre was also shut down.
...
... A police notice said the closure was on the orders of Israel's internal
... security minister on the grounds of a breach of interim peace accords
... from the 1990s.
...
... These laid the framework for talks on establishing a Palestinian state
... alongside Israel, but left the status of Jerusalem to be determined by
... further negotiation.
...
... Israel has annexed East Jerusalem and declares it part of its eternal
... capital.
...
... Palestinians hope to establish their capital in the area.'''
    
extract_keyword(text)    