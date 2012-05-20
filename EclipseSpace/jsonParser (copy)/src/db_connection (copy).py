import MySQLdb

def db_connect():
    DB = 'wallpost'
    DB_HOST = 'localhost'
    DB_USER = 'root'
    DB_PASSWORD = 'acer1986'
    try:
        conn = MySQLdb.Connection(db=DB, host=DB_HOST, user=DB_USER, passwd=DB_PASSWORD)
        return conn
    except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
        

def db_close (conn):
    try:
        conn.commit()
        conn.close()
    except MySQLdb.Error, e:
        print "Error %d: %s" % (e.args[0], e.args[1])
