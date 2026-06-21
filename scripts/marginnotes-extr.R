
  query <- "
SELECT
    n.ZNOTETITLE,
    n.ZHIGHLIGHT_TEXT,
    n.ZNOTES_TEXT,
    n.ZTOPICID,
    t.ZTITLE
FROM ZBOOKNOTE n
LEFT JOIN ZTOPIC t
    ON n.ZTOPICID = t.ZTOPICID
LIMIT 20;
"
  query <- "
SELECT
    n.ZNOTETITLE,
    n.ZHIGHLIGHT_TEXT,
    n.ZNOTES_TEXT,
    n.ZTOPICID,
    t.ZTITLE
FROM ZBOOKNOTE n
LEFT JOIN ZTOPIC t
    ON n.ZTOPICID = t.ZTOPICID
LIMIT 20;
"
#qa
query<-"
SELECT
    ZNOTETITLE,
    ZHIGHLIGHT_TEXT,
    ZNOTES_TEXT,
    ZTOPICID,
    ZTITLE
FROM ZBOOKNOTE 
LEFT JOIN ZTOPIC 
    ON ZTOPICID = ZTOPICID
LIMIT 20;
"
# AS query
# FROM sqlite_master n
# JOIN pragma_table_info(n.name) t
# WHERE n.type = 'table';

# "
  query <- "
SELECT 
    'SELECT \"' || m.name || '\" AS table_name, \"' || p.name || '\" AS column_name, * FROM \"' || m.name || 
    '\" WHERE \"' || p.name || '\" LIKE ''%' || ? || '%'';' AS query
FROM sqlite_master m
JOIN pragma_table_info(m.name) p
WHERE m.type = 'table';
"

q0<-dbGetQuery(con, query, params = list("string"))
q0
ql<-dbGetQuery(con, "SELECT ZTITLE, ZTOPICID FROM ZTOPIC LIMIT 5")
ql<-dbGetQuery(con, "
SELECT ZNOTETITLE, ZTOPICID, ZBOOKMD5
FROM ZBOOKNOTE
LIMIT 5
")
ql
ql<-dbGetQuery(con, "
SELECT *
FROM ZBOOKNOTE
-- WHERE ZNOTETITLE LIKE 'KI'
LIMIT 50
")
ql
names(ql)
q1<-ql[6,30]
q1
names(ql)
library(clipr)
write_clip(ql)
q1
q2<-dbGetQuery(con, sprintf("
SELECT *
FROM ZTOPIC
WHERE ZTOPICID='%s'
",q1))
q2
dbGetQuery(con, "
SELECT ZFILE, ZPATH, ZMD5LONG
FROM ZBOOK
WHERE ZMD5LONG='9281f55125c40cc1b1c41ac71e806f6f9281f55125c40cc1b1c41ac71e806f6f'
")
q2
names(q2)
x1<-q2[26]
x1
q3<-dbGetQuery(con, sprintf("
SELECT *
FROM ZBOOK
WHERE ZCURRENTTOPICID = '%s'
LIMIT 5;
",x1))
names(q3)
q3
q2
names(ql)
q<-
dbGetQuery(con, "
SELECT ZFILE, ZPATH, ZBOOKMD5
FROM ZBOOK
WHERE ZMD5='9281f55125c40cc1b1c41ac71e806f6f9281f55125c40cc1b1c41ac71e806f6f'
")
