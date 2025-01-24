library(RSQLite)
d<-dbDriver("SQLite")
d
library(DBI)
con<-dbConnect(d)
con
#dbListTables(con <- dbConnect(RSQLite::SQLite(), ":memory:"))
con<-dbConnect(d,"/Users/guhl/boxHKW/21S/DH/local/temp/MarginNotes.sqlite")
con
con<-dbConnect(d,"/Users/guhl/Documents/GitHub/SPUND-LX/szondi/WITprose/2025-01-23_FolioFF.sqlite3")
highlights<-dbGetQuery(con, "SELECT * FROM highlights")
highlights<-highlights[highlights$document_id==2,]
paths<-dbGetQuery(con, "SELECT * FROM ZBOOK")
highlight_tags
tables<-dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table';")
# SELECT name FROM sqlite_master WHERE type='table';
tags<-dbGetQuery(con, "SELECT * FROM tags")
wolf<-5517
tables
znotes<-dbGetQuery(con, "SELECT * FROM ZBOOKNOTE")
