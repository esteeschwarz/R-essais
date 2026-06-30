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
  dborigin<-"/Users/guhl/Library/Containers/QReader.MarginStudy.easy/Data/Library/Private Documents/MN4NotebookDatabase/0"
  f<-list.files(dborigin,pattern="MarginNotes.sqlite*",full.names=T)
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
  #global.t<-dbGetQuery(con,".schema")
  all.t<-dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table';")
  all.t
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
ql
#getwd()
#setwd("../../../../temp")
typeof(ql)
qx<-unlist(ql)
#writeLines(qx,"marginscheme.txt")
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
  qt
 qt<-qt[!is.na(qt)]
 qt<-unique(unlist(qt))
#  qd<-data.frame(abind(qs,along=1))
qt
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
  ZNOTE2<-all.tables[[4]]
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
  studies<-ZNOTE2$ZTITLE
  length(studies)
  length(unique(studies))
  unique(studies)
  m1<-studies=="nietzsche"
  m1<-grepl("nietzsche",studies)
  m1[is.na(m1)]<-F
  sum(m1)
  studies[m1]
  z1<-unique(ZNOTE2$ZMD5)
  length(z1)
  length(ZNOTE2[,1])
  length(unique(ZNOTE$ZBOOKMD5))
  m2<-ZNOTE2$ZMD5%in%ZNOTE$ZBOOKMD5
  sum(m2)
  topics<-ZNOTE$ZTOPICID
  #topics<-qdt$ZBOOKMD5
  t2<-qdt$column_name
  t3<-unique(t2)
  length(t3)
  length(qdt[,1])
  head(t3)
  head(topics)
  t4<-unique(topics)
  m1<-topics%in%t3
  t5<-ZNOTE[m1,]
  t6<-t5[!is.na(t5$ZHIGHLIGHT_TEXT),]
  length(unique(t2))
  t1<-ql[grepl("TOPIC",ql)]
  length(unique(topics))
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
  studies<-unique(dbnotes.s$study)
  m<-grep("^nietzsche$",studies)
  studies[m]
  m2<-dbnotes.s$study==studies[m]
  m2[is.na(m2)]<-F
  sum(m2,na.rm=T)
  su1<-dbnotes.s[m2,]
    #dbt<-merge(all.tables,check.rows=F,check.names=F)
  #save(dbnotes.s,file="/Users/guhl/db/marginnotescpt.16172.RData")
  #save(dbnotes.s,file=paste0(Sys.getenv("GIT_TOP"),"/SPUND-LX/marginnotescpt.16172.RData"))
  #m<-grep("Wolf|wolf",dbnotes.s$doc)
  #we<-dbnotes.s[m,]
  dbDisconnect(con)
  return(dbnotes.s)
}
#db.sf<-margindb
fetch.anno_dep<-function(dbsub){
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
get.clist<-function(){
  mn4<-Sys.getenv("MN4")

  nietzsche<-"~/Library/Mobile Documents/iCloud~QReader~MarginStudy~easy/Documents/MN3/A_UNI/SZONDI/nietzsche"
nietzsche<-paste0(mn4,"/SZONDI/nietzsche")
  nietzsche
litKI<-paste0("~/Library/Mobile Documents/iCloud~QReader~MarginStudy~easy/Documents/MN3/A_UNI/SZONDI/lit-KI")
litKI<-paste0(mn4,"/SZONDI/lit-KI")
  
#stratling<-paste0(cloud,"SZONDI/strätling")
stratling<-"~/Library/Mobile Documents/iCloud~QReader~MarginStudy~easy/Documents/MN3/A_UNI/SZONDI/strätling"
stratling<-paste0(mn4,"/SZONDI/strätling")
#textur<-paste0(cloud,"SZONDI/textur")
  textur<-"~/Library/Mobile Documents/iCloud~QReader~MarginStudy~easy/Documents/MN3/A_UNI/SZONDI/textur"
  textur<-paste0(mn4,"/SZONDI/textur")
#LXtech<-paste0(cloud,"COMP/LX-tech")
#LFG<-paste0(cloud,"COMP/LFG")
LXtech<-"~/Library/Mobile Documents/iCloud~QReader~MarginStudy~easy/Documents/MN3/A_UNI/COMP/LX-tech"
  LXtech<-paste0(mn4,"/COMP/LX-tech")
  LFG<-"~/Library/Mobile Documents/iCloud~QReader~MarginStudy~easy/Documents/MN3/A_UNI/COMP/LFG"
  LFG<-paste0(mn4,"/COMP/LFG")
clist<-list(litKI=litKI,nietzsche=nietzsche,VSstr=stratling,textur=textur,LXtech=LXtech,LFG=LFG)
  clist
}
idb<-function(){
  library(RSQLite)
  d<-dbDriver("SQLite")
  d
  library(DBI)
  con<-dbConnect(d)
  con
#  dbsrc<-"/Users/guhl/Documents/temp/MarginNoteBackup(2025-02-01-13-53-27).marginbackupall"
 # dbsrc<-paste(dbsrc,"MarginNotes.sqlite",sep = "/")
  dborigin<-"/Users/guhl/Library/Containers/QReader.MarginStudy.easy/Data/Library/Private Documents/MN4NotebookDatabase/0/MarginNotes.sqlite"
  dborigin<-"/Users/guhl/Library/Containers/QReader.MarginStudy.easy/Data/Library/Private Documents/MN4NotebookDatabase/0"
  f<-list.files(dborigin,pattern="MarginNotes.sqlite*",full.names=T)
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
  #global.t<-dbGetQuery(con,".schema")
  all.t<-dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table';")
  all.t
  library(abind)
  all.tg <<- all.t
  all.tables<-lapply(seq_along(1:length(all.t$name)),function(i){
    t<-dbGetQuery(con,paste0("SELECT * FROM ",all.t$name[i],";"))

                  
  })
    dbDisconnect(con)
  return(all.tables)

}

get.margin<-function(qa){
  all.tables<-idb()
  # clist<-get.clist()
  #clist
  clist<-qa
  all.t<-all.tg
  library(dplyr)
  tabc<-lapply(seq_along(all.tables),function(i){
    print(i)
    x<-all.tables[[i]]
    head(x)
    if(length(x[,1])>0)
      x$table<-all.t$name[i]
    return(x)
  })
  tabd<-bind_rows(tabc)
  m<-colnames(tabd)=="table"
  tabd<-tabd[,c(which(m),which(!m))]


#h1<-head(tabd[!is.na(tabd$ZHIGHLIGHT_TEXT),],10)
h1<-tabd[!is.na(tabd$ZHIGHLIGHT_TEXT),]
#h2<-h1[!is.na(h1[,1:length(h1)]),]
mn<-!is.na(h1[,1:length(h1)])
sum(mn)
h3<-t(mn)
#mn
#m1<-h3[,1:length(h3)]==T
rowSums(mn)
s<-apply(mn,2,sum)
h4<-t(h1)
s2<-t(s)
s2
length(h1)
h5<-h1[,c(which(s2==length(h1[,1])))]
#View(h5)
t1<-h5$ZTOPICID
length(unique(t1)) # 853 topics
########################################
  #s3<-tabd[t1%in%tabd[,1:length(tabd)]]
t1
unique(tabd$table)
zbook<-tabd[tabd$table=="ZBOOK",]
colnames(zbook)
ztitle<-tabd[!is.na(tabd$ZTITLE),]
ztitles<-unique(ztitle$ZTITLE)
length(ztitles) # 4780
m<-ztitle$ZTITLE%in%names(clist)
sum(m)
cnote<-ztitle[m,]
  cnote
studies<-ztitle$ZTITLE[m]
studies
ctopicid<-cnote$ZTOPICID
  dim(tabd)
typeof(tabd)
#tabl<-lapply(tabd,unlist)
l1<-apply(tabd,2,function(i){length(unlist(i))})
m<-l1==dim(tabd)[1]
td<-data.frame(tabd[,which(m)])
#mx<-ctopicid%in%td[,1:length(td)]
#dim(tm)
ctopicid
# mt<-lapply(ctopicid,function(x){

# mx<-apply(td,2,function(i){
#   m<-grep(x,i)
#   ifelse(length(m)!=0,m,F)
#   })
#   m<-unlist(mx)
#   ifelse(length(m)!=0,m,F)
# })
#dim(mx)
#mx
#mx[mx!=0]
#ts<-td[c(mx[mx!=0]),]
#tu<-unique(td$ZTITLE)
#tu
t5<-td[td$ZTOPICID%in%ctopicid,]
t5b<-td[td$ZCURRENTTOPICID%in%ctopicid,]
t5c<-tabd[td$ZTOPICID%in%ctopicid,]
t5d<-tabd[td$ZCURRENTTOPICID%in%ctopicid,]
  colnames(t5)
t6<-t5[!is.na(t5$ZHIGHLIGHT_TEXT)|!is.na(t5$ZNOTES_TEXT),]
t9<-t5[t5$table=="ZBOOKNOTE",]
books<-t5$ZBOOKMD5
length(unique(books))
t6<-td[td$ZBOOKMD5%in%unique(books)&td$ZTOPICID%in%ctopicid,]
t6<-td[td$ZBOOKMD5%in%unique(books),]
t6$study<-NA
# for (k in 1:length(cnote$ZTITLE)){
#   sid<-cnote$ZTOPICID[k]
#   m<-t6$ZTOPICID==sid
#   m[is.na(m)]<-F
#   t6$study[m]<-cnote$ZTITLE
# }
t6<-t6[!is.na(t6$ZBOOKMD),]
unique(t6$table)
  colnames(t6)
#t7<-td[td$table=="ZBOOK",]
t8<-td[td$ZBOOKMD5%in%unique(books)|td$Z%in%unique(books),]
colnames(t8)
m1<-lapply(td[,1:length(td)],function(x){
  m1<-grep(".pdf",x)
  #print(colnames(x))
#  print(m1)
  ifelse(length(m1)>0,p<-m1,p<-NA)
  return(p)
})
  ### wks.
# m2<-lapply(t6[,1:length(td)],function(x){
#   m1<-grep(".pdf",x)
#   #print(colnames(x))
#   print(m1)
#   ifelse(length(m1)>0,p<-m1,p<-NA)
#   return(p)
# })
m3<-!is.na(m1)
sum(m3)
m2<-m1[m3]
m4<-unique(unlist(m2))
length(m4)
t8<-td[m4,]
  unique(t8$table)
t9<-bind_rows(t6,t8)
cn<-colnames(t9)
t9$study<-NA
cn<-colnames(t9)
k<-1
for (k in 1:length(cnote$ZTITLE)){
  sid<-cnote$ZTOPICID[k]
  m<-t9$ZTOPICID==sid
  sum(m,na.rm=T)
  m[is.na(m)]<-F
  t9$study[m]<-cnote$ZTITLE[k]
}
  colnames(cnote)
  cn
  cg<-grep("NOTE|HIGHLIGH|MD5|FILE|PATH|URL|TEXT|COMMENT|PAGE|TITLE|TAG|TOPIC|TIMESTAMP|table|study",cn)
  cg<-unique(cg)
  t10<-t9[,cg]
  t11<-t10[order(t10$ZTOPICID,t10$ZBOOKMD5,t10$ZSTARTPAGE),]
  s<-unique(t11$study)
  s<-s[!is.na(s)]
  s
  t11md5<-t11$ZBOOKMD5
  t11md5l<-t11$ZMD5LONG
  u1<-unique(t11md5l)
  u1
  md5u<-unique(t11md5)
  length(md5u)
  #x
#  t11<-t11[!is.na(t11$ZNOTEID),]
  pns<-lapply(md5u,function(x){

  mc<-lapply(t11[,1:length(t11)],function(c){
    #x%in%c
    m<-c%in%x
    #t11$ZBOOKMD5&t11$table=="ZBOOK"
    m[is.na(m)]<-F
    ifelse(sum(m)>0,p<-which(m),p<-NA)
    return(p)
    
  })
    mcc<-mc[!is.na(mc)]
#    mcc<-mc[unlist(mc)]
    mcc
  })
  colnames(t11)
  t11$doc<-NA
  for (k in 1:length(pns)){
    md5<-pns[[k]]$ZMD5LONG
    d<-t11$ZFILE[md5]
    if(is.null(d))
      d<-""
    if(length(d)==0)
      d<-""
#    d<-unique(d)
    p<-pns[[k]]$ZBOOKMD5
    #m<-t11$ZBOOKMD5%in%p
    if(length(p)>0)
      t11$doc[p]<-d
  }
  ### wks.
  s<-studies
  s
  k<-s[5]
  k
  s
  colnames(t11)
  ##############
  ### debug stop
  ##############
  for(k in s){
    m<-t11$study==k
    sum(m,na.rm=T)
    m[is.na(m)]<-F
    d1<-unique(t11$doc[m])
    d1
    m2<-t11$doc%in%d1
    sum(m2)
    # m3<-is.na(t11$study[m2])
    # sum(m3)
    # sum(!m3)
########################
    ### ensure book is in study
    t11b<-t11[m,]
    m4<-grepl("#notebook_",t11b$ZNOTES_TEXT)
    m5<-grep("#notebook_",t11b$ZNOTES_TEXT)
    length(m5)
    #m4<-grepl("kook",t11$doc)
    #s1<-t11[m4,]
    m4[is.na(m4)]<-F
    sum(m4)
    t11b[m5,]
    doc.out<-t11b$doc[m5]
    m6<-t11$doc==doc.out
    #t11[which(m4),]
    #if(sum(m4)==0){
   # s1
    m7<-which(m2)
    m7
    da<-t11$doc
    da
    dm<-da%in%doc.out
    sum(dm)
    da[dm]
    sum(is.na(m2))
    m8<-m2[which(!dm)]
    #m9<-
    length(m8)
    ### glitch
    unique(t11$study[m8])
    m3<-is.na(t11$study[m8])
    sum(m3)
    sum(!m3)
  
    sum(m3)
    t12<-t11[m8,]
    t12$ZHIGHLIGHT_TEXT
    t11[m8,]
    cat("--- reapplied",sum(m8),"changes to study -",k,"- names according to books ---\n")
    t11$ZHIGHLIGHT_TEXT[m8]
    t11$study[m8]<-k
    #}
  }#x<-t11[,1]
  cg
  cns<-colnames(t11)
  cns
  #c<-cns[1]
  t11l<-lapply(cns,function(c){
    x<-t11[,c]

    e<-x==""
    x[e]<-NA
    t<-sum(is.na(x))==length(x)
    ifelse(t,r<-NA,r<-data.frame(c=x))
   # print(c)
    if(!t)
      colnames(r)<-c
    return(r)
  })
  cat("--- t12 ---\n")
  t12<-t11l[!is.na(t11l)]
  t13<-data.frame(abind(t12,along=2))
  t14<-t13[!is.na(t13$doc),]
  mode(t14$ZSTARTPAGE)<-"numeric"
  mode(t14$ZENDPAGE)<-"numeric"
  t14<-t14[order(t14$doc,t14$ZSTARTPAGE,t14$ZENDPAGE),]
  sum(is.na(t14$ZNOTEID))
  colnames(t14)
  cn<-c(27,28,15,14,21,22,23,24,1,2,3)
  margin1<-t14[,cn]
  cns<-c("study","doc","spage","epage","notes","comment","title","ocr","table","nid","tid")
  colnames(margin1)<-cns
  #docs<-unique(margin1$doc)
  #docs<-docs[!is.na(docs)]
  studies
  x<-studies[6]
  x
  qax<-lapply(studies,function(x){
    m<-margin1$study==x
    sum(m,rm.na=T)
    d<-unique(margin1$doc[m])
    d<-d[!is.na(d)]
    d2<-d
    
    if(length(d2)>0)
      names(d2)<-x
    r<-list(q=d)
    print(x)
    #names(r)<-x
    return(d)

  })
  names(qax)<-studies
  return(list(qa=qax,dbsub=margin1))
#################
  t11$ZMD5LONG[is.na(t11$ZMD5LONG)]<-F
  px<-t11[mcc$ZMD5LONG,]
  x<-md5u[3]
  x<-c("a","b","c")
  xa<-list(names=x)
  names(xa)<-"to"
  xa
  m<-x==t11$ZBOOKMD5
  m[is.na(m)]<-F
  tm5<-t11[m,]
  sum(m)
  pns
t9$ZBOOK
pdfs<-unique(t6$ZBOOKURL)
pdfs
  m<-tu=="LFG"
which(m)
t3<-ts[!is.na(ts$ZTITLE),]
t3<-t3[,!is.na(t3[,1:length(t3)])]
t3
qa<-get.clist()
margin1<-get.margin(qa)
  getwd()
#margin2<-margin1
save(margin1,file="margin1.RData")
load("margindb.RData")
names(margindb)
  dbsub<-margindb$dbsub
m<-grep("kook",dbsub$doc)
  dbsub$notes[m]
length(m)
}