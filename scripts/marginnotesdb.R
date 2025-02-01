tempfun<-function(){
  library(RSQLite)
  d<-dbDriver("SQLite")
  d
  library(DBI)
  con<-dbConnect(d)
  con
  dbsrc<-"/Users/guhl/Documents/temp/MarginNoteBackup(2025-02-01-13-53-27).marginbackupall"
  dbsrc<-paste(dbsrc,"MarginNotes.sqlite",sep = "/")
  #dbListTables(con <- dbConnect(RSQLite::SQLite(), ":memory:"))
  con<-dbConnect(d,dbsrc)
  #con<-dbConnect(d,"/Users/guhl/boxHKW/21S/DH/local/AVL/2024/WIT/2025-01-21_FolioFF.sqlite")
  #con<-dbConnect(d,"/Users/guhl/Documents/GitHub/SPUND-LX/szondi/WITprose/2025-01-23_FolioFF.sqlite3")
  #highlights<-dbGetQuery(con, "SELECT * FROM highlights")
  #highlights<-highlights[highlights$document_id==2,]
  #highlight_tags<-dbGetQuery(con, "SELECT * FROM highlight_tags")
  all.t<-dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table';")
  library(abind)
  all.t
  all.tables<-lapply(seq_along(1:length(all.t$name)),function(i){
    t<-dbGetQuery(con,paste0("SELECT * FROM ",all.t$name[i],";"))
                  
  })
  #wks.
  i<-1
  ZFILE<-all.tables[[2]]
  ZNOTE<-all.tables[[5]]
  ZNOTE[1,]
  all.notes<-lapply(seq_along(1:length(ZFILE$Z_PK)),function(i){
    bookrow<-ZFILE[i,]
    bookrow
    bookmd5<-bookrow$ZMD5LONG
    booktitle<-bookrow$ZFILE
    m<-bookmd5==ZNOTE$ZBOOKMD5
    sum(m)
#    rlist<-list()
 #   rlist[[booktitle]]<-bookmd5
  #  rlist[["notes"]]<-ZNOTE$ZNOTES_TEXT[m]
    rdf<-matrix(ncol=7,nrow=1,c(booktitle,"#NO-ANN#",1:5))
    if(sum(m)>0)
      rdf<-data.frame(doc=booktitle,notes=ZNOTE$ZHIGHLIGHT_TEXT[m],
                    comment=ZNOTE$ZNOTES_TEXT[m],spage=ZNOTE$ZSTARTPAGE[m],epage=ZNOTE$ZENDPAGE[m],
                    xys=ZNOTE$ZSTARTPOS[m],xye=ZNOTE$ZENDPOS[m])
    
    return(rdf)

  })
  #?abind
  dbnotes<-data.frame(abind(all.notes,along = 1))
  
  dbnotes.s<-dbnotes[order(dbnotes),]

    dbt<-merge(all.tables,check.rows=F,check.names=F)
  save(dbnotes.s,file="/Users/guhl/Documents/temp/marginnotescpt15061.RData")
  m<-grep("Wolf|wolf",dbnotes.s$doc)
  we<-dbnotes.s[m,]
  
  