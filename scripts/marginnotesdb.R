get.notes<-function(qa){
  library(RSQLite)
  d<-dbDriver("SQLite")
  d
  library(DBI)
  con<-dbConnect(d)
  con
#  dbsrc<-"/Users/guhl/Documents/temp/MarginNoteBackup(2025-02-01-13-53-27).marginbackupall"
 # dbsrc<-paste(dbsrc,"MarginNotes.sqlite",sep = "/")
  dborigin<-"/Users/guhl/Library/Containers/QReader.MarginStudy.easy/Data/Library/Private Documents/MN4NotebookDatabase/0/MarginNotes.sqlite"
  dborigin<-"/Users/guhl/Library/Containers/QReader.MarginStudy.easy/Data/Library/Private Documents/MN4NotebookDatabase/0/"
  f<-list.files(dborigin,pattern="MarginNotes.sqlite",full.names=T)
  f
  dbcopy<-"~/db/MarginNotes.sqlite"
  dbcopy<-"~/db/marginnotes/"
  #file.copy(dborigin,dbcopy)
  file.copy(f,dbcopy,overwrite=T)
  dbsrc<-paste0(dbcopy,"MarginNotes.sqlite")    
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
  query <- "
SELECT 
    'SELECT \"' || m.name || '\" AS table_name, \"' || p.name || '\" AS column_name, * FROM \"' || m.name || 
    '\" WHERE \"' || p.name || '\" LIKE ''%' || ? || '%'';' AS query
FROM sqlite_master m
JOIN pragma_table_info(m.name) p
WHERE m.type = 'table';
"
qa
ql<-dbGetQuery(con, query, params = list("string"))
ql<-dbGetQuery(con, query, params = list(names(qa)))
#x<-unlist(ql)[172]
qs<-lapply(unlist(ql),function(x){
  print(x)
  r<-dbGetQuery(con, x)
  print(dim(r))
  ifelse(dim(r)[1]>0,d<-r,d<-NA)
  return(d)
})
library(abind)

qs<-qs[!is.na(qs)]
qt<-lapply(qs,function(x){
  c<-grep("TOPICID",colnames(x))
  t<-ifelse(length(c)>0,unique(x[,c]),NA)
})
 qt<-qt[!is.na(qt)]
 qt<-unique(unlist(qt))
#  qd<-data.frame(abind(qs,along=1))

#tid<-qd$ZTOPICID
qlt<-dbGetQuery(con, query, params = list(qt))
qst<-lapply(unlist(qlt),function(x){
  print(x)
  r<-dbGetQuery(con, x)
  print(dim(r))
  ifelse(dim(r)[1]>0&sum("ZBOOKNOTE"%in%r$table_name,"ZHIGHLIGHT_TEXT"%in%colnames(r))==2,d<-r,d<-NA)
  
  #ifelse(!d,d<-r,d<-NA)
  return(d)
})
qstn<-qst[!is.na(qst)]
  length(qstn)

    library(dplyr)

  qdt<-bind_rows(qstn)
#return(qdt)
#}
booksmd5<-unique(qdt$ZBOOKMD5)
booksmd5
#  lx<-dbGetQuery(con,query)
  ZFILE<-all.tables[[2]]
  ZNOTE<-all.tables[[5]]
  #tu<-unique(ZNOTE$ZTOPICID)
  #length(tu)
  ZTITLE<-all.tables[[11]]
  #m<-ZTITLE$ZTITLE=="LXtech"
  #mb<-ZTITLE[m,]
  #mbooks<-mb$ZLOCALBOOKMD5
  #mbooks<-strsplit(mbooks,split="|",fixed=T)
  booksmd5
  mf<-ZNOTE[ZNOTE$ZBOOKMD5%in%booksmd5,]
  mf
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
    rdf<-matrix(ncol=9,nrow=1,c(booktitle,"#NO-ANN#",1:6))
    if(sum(m)>0)
      rdf<-data.frame(doc=booktitle,study=ZNOTE$ZNOTETITLE[m],notes=ZNOTE$ZHIGHLIGHT_TEXT[m],
                    comment=ZNOTE$ZNOTES_TEXT[m],spage=ZNOTE$ZSTARTPAGE[m],epage=ZNOTE$ZENDPAGE[m],
                    xys=ZNOTE$ZSTARTPOS[m],xye=ZNOTE$ZENDPOS[m],md5=bookmd5)
    
    return(rdf)

  })
  #?abind
  dbnotes<-data.frame(abind(all.notes,along = 1))
  
  dbnotes.s<-dbnotes[order(dbnotes$doc),]

    #dbt<-merge(all.tables,check.rows=F,check.names=F)
  #save(dbnotes.s,file="/Users/guhl/db/marginnotescpt.16172.RData")
  #save(dbnotes.s,file=paste0(Sys.getenv("GIT_TOP"),"/SPUND-LX/marginnotescpt.16172.RData"))
  #m<-grep("Wolf|wolf",dbnotes.s$doc)
  #we<-dbnotes.s[m,]
  dbDisconnect(con)
  return(dbnotes.s)
}
#db.sf<-margindb
fetch.anno<-function(dbsub){
  load(paste0(Sys.getenv("GIT_TOP"),"/SPUND-LX/play/quarto/start/margindb.RData"))
  #margindb<-dbsub
  # qa<-list(litKI="Wiener_Einführung.pdf",nietzsche=c("nietzsche, kga 3-1, geburt d tragödie.pdf","nietzsche briefe ggl.pdf"),textur=NA) # margin note studyset
x<-qa[[1]]
ql<-lapply(seq_along(qa), function(x){
  study<-names(qa[x])
  print(study)
  pl<-lapply(qa[[x]], function(p){
    paper<-p
    print(paper)
    m1<-grep(paper,margindb$doc)
    print(length(m1))
    mi<-grep("#init",margindb$comment)
    print(margindb$notes[mi])
    mi<-mi[mi%in%m1]
    print(margindb$comment[mi])
    ci<-gsub("#cite_(.*)_","\\1",margindb$comment[mi])
    print(ci)
    mt<-grep("#init",margindb$comment)
    ct<-gsub("#init_(.*)_","\\1",margindb$comment[mt])
    pn<-margindb[m1,]
    pn<-pn[order(pn$spage),]
    pn$title<-ct
    pn$cite<-ci
    pn$study<-study
    return(pn)
    
  })
})
  
    
}


  