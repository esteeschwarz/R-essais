#13336.corpus essai: benjaminfeldkraft
#20230817(17.04)
#README:
# the corpus text of ~200.000 wds was originally written in 5 books, which
# have been conglomerated for editorial reasons into one i.e. the books have been
# merged. afterwards the text has been lectured in that conglomerated version and the 
# originating 5 texts left aside.
# now for again editorial reasons we wanted to have the merged version again splitted
# into the 5 books.
# this script does a tokenization of the originals, compares 5-ngrams of that texts to
# paragraphs/chapters in the merged version and assignes these paragraphs to the number of the book
# it originates from. then it outputs html and md versions of the 5 different books, now
# in the stadium of the lectorised version. finally it creates a list-summary.md of the book to use for
# [mdbook] creation to display the book in a browser.
################

library(xml2)
library(jsonlite)
library(XML)
library(rvest)
library(quanteda)
library(readr)
library(clipr)
library(stringi)
####
setwd("~/boxHKW/21S/DH/local/SPUND/corpuslx/benjamin")

fun.dep<-function(){
xhtm<-read_html("benjamin12.html",encoding = "UTF-16") # sonderzeichen messed
all.p<-xml_find_all(xhtm,"//p")
xml_text(all.p[10])
all.i<-xml_find_all(xhtm,"//em")
all.h<-xml_find_all(xhtm,"//h2")
xml_text(all.i)
all.hn<-xml_find_one(xhtm,"//h2",flatten=T)
which(all.i)
all.top<-xml_find_all(xhtm,"//*")
which(xml_find_first(xhtm,"//em"))
xhtm<-read_html("benjamin12.html",encoding = "UTF-16") # sonderzeichen messed
xml_name(all.top[[6]])
xml_text(all.top[[3]]) #first p element
all.h.nr<-which(xml_name(all.top)=="h2") #THIS: returns all h2 nodes
xml_text(all.top[21])
#wks.

h1<-html_nodes(xhtm,"h2")
as_xml_document(doc0)
doc6<-xml_new_root("body",doc0)
doc6<-xml_add_child(doc6,doc0)


hn.1<-all.h.nr-1
#3:all.h.nr[1]
doc0<-(all.top[3:hn.1[1]])
for (k in all.h.nr){
  doc1<-xml_text(all.top[])
}
doc0
write_html(all.top[3:4],"testtitle.html")
writeLines(all.top[3],"testtitle.html")
doc2<-unlist(all.top[3:hn.1[1]])
doc0
doc2<-as.data.frame(doc0)
doc2<-list()
doc2[["body"]]<-doc0
doc3<-read_html(doc2)

write_html(doc2,"testtitle.html")


h <- read_html(c("<p><em>Hi!</em></p>","<p>normal</p>"))

tmp <- tempfile(fileext = ".xml")
write_xml(h, tmp, options = "format")
readLines(tmp)

top <- xmlNode("top", xmlNode("next","Some text"))
#top[["second"]] <- xmlCDataNode("x <- 1:10")
top[[3]] <- xmlNode("tag",attrs=c(id="name"))
top
doc4<-read_html(top)
cat(as(toHTML(top),"character"))
write_html(toHTML-method(top),"testhtml.html")
saveXML(doc0,"testhtml.html")
doc5<-xmlParse("testhtml.html")
doc5[[1]]

x1<-read_xml("benjaminfeldkraft12.epub") # sonderzeichen messed
x2<-fromJSON("json/titlePageContent.json",flatten = T)
all.p<-xml_find_all(x2,"//p")

k<-2
files.xml<-list.files("xml")
for (k in files.xml){
  x<-read_html(files.xml[k])
  all.p<-xml_find_all(x,"//p")
  all.i<-xml_find_all(x,"//i")
  xml_text(all.p)
  xml_text(all.i)
  
    txhead<-tx1[1]
  
  writeLines(tx1[2:length(tx1)]),paste0(txhead,"")
}
x<-read_html("xml/titlePageContent.xhtml") # sonderzeichen messed
all.p<-xml_find_all(x,"//p")
xml_text(all.p)

ll <- list(1:4, 5:6, 7:12, 1:12)
ll <- lapply(ll, as.character)


ll<-xhtm
body<-html_node(xhtm,"body")
ll<-body
ll<-all.top
which(sapply(ll, FUN=function(X) "h2" %in% X))
# [1] 3 4

ll <- list(c("7", "12", "26", "29"),
           c("11", "36"),
           c("20", "49"),
           c("39", "41"))
ll<-body
ll<-all.top.u
df <- data.frame(value = unlist(ll),
                 index = rep(seq_along(ll), lapply(ll, length)))
df
all.top.u<-unlist(all.top)

df$Name <- xml_text(xml_find_all(test_xml2, "//XML_Name"))
a1<-all.top[[10]]
a2<-html_name(a1)
a3<-xml_text(a1)
length(all.top)
doc1<-xml_new_document()
xml_add_child(doc1,a1)
doc1
k<-10
mh<-grep("h2",html_name(all.top))
for(k in 3:length(all.top)){
  a1<-all.top[[k]]
  a2<-html_name(a1)
  if(a2=="p"){
  xml_text(a1)
  #xml_add_child(doc1,a2)
  #xml_text()
  
  }
}
mh.end<-mh-1
doc1
doc1<-xml_new_document()
xml_add_child(doc1,"body")
xml_add_child(xml_children(xhtm),"h2")
doc1
n.curr<-xml_find_all(doc1,"//body")
xml_text(doc1[1])
for (k in mh){
  
  
  
}
write_html(xhtm,"testhtml.html")  
write_xml(all.top,"testhtml.html")  
length(xhtm)
xhtm2<-xhtm
xml_add_child(
)
all.h
xhtm2[[2]][11]
all.p<-xml_find_all(xhtm,"//p")
xml_text(all.p[1])
all.i<-xml_find_all(xhtm,"//em")
all.h<-xml_find_all(xhtm,"//h2")
all.h[1]
xml_add_child(all.h[1],"p")
h1p<-xml_find_all(all.h,"p")

k<-10
mh<-grep("h2",html_name(all.top))
p.end<-mh-1
p.start<-mh+1
p.start<-c(3,p.start)
#which(all.top,html_name(all.top)=="p")
xhtm2<-read_html("benjamin13.html",encoding = "UTF-16") # sonderzeichen messed
write_xml(doc0,"testwrite.html")
all.top<-xml_find_all(xhtm,"//*")
all.p<-xml_find_all(xhtm,"//p")

xml_text(all.top[11])
h<-1
xhtm2<-xhtm
body<-xml_find_all(xhtm2,"//body")
doc0<-xml_new_document()
xml_node(doc0,"body")
xml_remove(body)
h<-1
for (h in 1:length(mh)){
  cat ("heading:",h,"\n")
  
#mp<-grep("p",)
  k<-h
  lp<-p.start[k]:p.end[k]
  lp<-length(lp)
  #for (e in 1:length(lp)){
    xml_add_child(xhtm2,"h3")
    h3<-xml_find_all(xhtm2,"//h3")
    e<-1
    for (e in 1:lp){
    xml_add_child(h3,"p")
    hp<-xml_find_all(h3,"//p")
    xml_text(hp[e])<-xml_text(all.p[lp])
    cat("paragraph",e,"\n")
  }
    write_xml(hp,paste0("expo/ben_ch_",h,".html"))
    cat("written\n")
}
body<-xml_find_all(xhtm2,"body")
xml_remove(body)

write_html(xhtm2,"testwrite.html")

for(k in 3:length(all.top)){
  a1<-all.top[[k]]
  a2<-html_name(a1)
  if(a2=="p"){
    xml_text(a1)
    #xml_add_child(doc1,a2)
    #xml_text()
    
  }
}
mh.end<-mh-1
}#end dep
############### neu
#t1<-readLines("benjaminbuch1.html")
#booknr<-2

getbook<-function(booknr){
df1<-df.x
df1$book[1:6]<-5 # manual define beginning, title page

    book.n<-booknr
  file<-paste0("benjaminbuch",book.n,".html")
t1<-read_html(file,encoding = "UTF-16")
#book.n<-1
all.p<-xml_find_all(t1,"//p")
all.p.t<-xml_text(all.p)
all.p.tok<-tokens(all.p.t,"word",remove_punct = T)
all.p.tok[20]
all.p.tok.c<-tokens_chunk(all.p.tok,5)
all.p.tok.c[20]
all.p.tok.n<-tokens_ngrams(all.p.tok,5,concatenator = " ")

tok_l<-function(x) length(x)
t.l<-lapply(all.p.tok.n, tok_l)
m.l<-t.l>=5
sum(m.l)
all.p.tok.n.l<-all.p.tok.n[m.l]
all.p.tok.n.l[2]
tok_l<-function(x) length(x)
t.l<-lapply(all.p.tok.c, tok_l)
m.l<-t.l>=5
sum(m.l)
all.p.tok.c.l<-all.p.tok.c[m.l]



#length(all.p.tok.c[59])


#df1<-read.csv("benjamin.DF.csv")
###
#all.p.tok.c[20]
#df1.save<-df1
#df1$book<-NA
#k<-20
#df2.save<-df1
#df1<-df1.save
###check
#regx<-gsub("(\\(|\\)|\\|[\\]|\\{|\\})","\\//1",all.p.tok.c[k])
#regx<-paste0(regx,collapse = " ")
#rg<-"gelassne Weisheit"
#m.e<-grep(rg,df1$p)
#m.m<-array()
# LAPPLY:
#codef<-function(x) stri_extract_all_regex(x,"(#[A-Z]{3})")
#ms7<-lapply(sent1, codef)
#m<-length(all.p.tok.c[[1:10]])<=5
###
#tok_l<-function(x) length(x)
#t.l<-lapply(all.p.tok.c, tok_l)
#m.l<-t.l>=5
#sum(m.l)
#all.p.tok.c.l<-all.p.tok.c[m.l]
#all.p.tok.c.s<-
# for (k in 1:length(all.p.tok.c.l)){
#   regx<-gsub("(\\(|\\)|\\|[\\]|\\{|\\})","\\//1",all.p.tok.c.l[k])
#   regx<-paste0(regx,collapse = " ")
#   m<-grep(regx,df1$p[m.e])
#   
#   print(k)
#   print(m)
#   if(sum(m)!=0)
#     m.m[k]<-m
# }
# m.w<-which(!is.na(m.m))
# all.p.tok.c[m.w]
# length(all.p.tok.c[[1]])
# t.test<-all.p.tok.c[[1]]
# write_clip(paste0(t.test,collapse = " "))
r.1<- "und nähme Ihre Worte die"
r.1<-"und zwar in einem Manuskript"
r.1<-"Zum Beispiel in Worten"
# sum(m.m,na.rm = T)
###
#k<-219
#df1$book<-NA
#remove punctuation from df$p
df1$p_p<-gsub("[.;,:-]","",df1$p)
#p.g<-gsub("[.;,:-]","",df1$p[7])
#k
#m
for (k in 1:length(all.p.tok.n.l)){
  regx<-gsub("(\\(|\\)|\\|[\\]|\\{|\\})","\\//1",all.p.tok.n.l[k])
#STEPS
  #ngrams.s<-seq(from = 1, to = length(ngrams.xu), by = 8)
  regx.s<-seq(from = 1, to = length(regx), by = 5)
regx<-regx[regx.s]
r.s<-sample(regx,10,replace = T)
#  regx<-paste0(regx,collapse = " ")
#regx<-r.1
m.a<-array()    
    for (mk in 1:length(r.s)){
      m.g<-grep(r.s[mk],df1$p_p)
      if(sum(m.g)>0)
            m.a[mk]<-m.g
    }
m.a
m.a.u<-unique(m.a)
m.a.u<-m.a.u[m.a.u!=0]
m.a.u<-m.a.u[!is.na(m.a.u)]
m.a.u    
#m<-grep(regx,df1$p_p)
df1$book[m.a.u]<-book.n  
print(k)
}
#grep("Worten",all.p.tok.c.l)
#all.p.tok.n.l[20]
# getwd()
# df1$h2[2042:2084]<-145
# df1$h2[2085:length(df1$h2)]<-146
# chapter 145, 146: footnotes
# df1$book[df1$h2==145|df1$h2==146]<-NA
#csv.ns<-paste0("benjamin13.DF-",book.n,".csv")
#write_csv(df1,"benjamin.DF.2.csv")
#write_csv(df1,csv.ns)
#} #end getbook function

#regx
#df1$p[m]
#sum(df1$book!=1,na.rm = T)
#sum(is.na(df1$book))
book.n
h2.not.1<-df1$book!=book.n
h2.not.1[is.na(h2.not.1)]<-T
unique(h2.not.1)

tail(df1$h2)
length(unique(df1$h2))
### write single file for chapters
ch.u<-unique(df1$h2)
ch.u<-ch.u[1:144]
ben.ns<-"benjamin13.2-"

dir.out.html<-paste0("expo/",ben.ns,book.n,"-htm")
dir.create(dir.out.html)
#h2.n<-df1$h2[!is.na(df1$book)]
#h2.n.u<-unique(h2.n)
#ch.u<-ch.u[ch.u.b==T]
#k<-2
book.n
# ben.ns<-"/benjamin13.2-"
dir.gl.md<-paste0("expo/",ben.ns,"md")
dir.create(dir.gl.md)
dir.gl.htm<-paste0("expo/",ben.ns,"htm")
dir.create(dir.gl.htm)
book.n
k

### important loop to fill in book NAs
chose<-df1$h2==ch.u[k]&df1$book==book.n
chose.na<-is.na(df1$book)
sum(chose.na,na.rm = T)
tail(which(chose.na))
###move backwards to table
lastna<-tail(which(chose.na))[6]
b<-lastna
for (b in lastna:1){
  b.2<-lastna.2
  if(is.na(df1$book[b.2])){
    back.max<-b.2
    back<-b.2-back.max
    back.na.m<-!is.na(df1$book[b.2:back]) # where is the next !na looking backwards to start
    back.na.m.w<-which(back.na.m==T)[1]-1 # position (distance) from actual lastna.2
    lastna<-lastna.2
    ifelse(df1$h2[lastna]==df1$h2[lastna+1],
           df1$book[lastna]<-df1$book[lastna+1],
           if(df1$h2[lastna]==df1$h2[lastna-1])
             df1$book[lastna]<-df1$book[lastna-1])
    if(is.na(df1$book[lastna-1]))
      df1$book[lastna]<-df1$book[lastna-2]
    ifelse(df1$h2[lastna]==df1$h2[lastna-back.na.m.w]&
             df1$h2[lastna]==df1$h2[lastna+1],
           df1$book[lastna]<-df1$book[lastna-back.na.m.w],
           df1$book[lastna]<-df1$book[lastna+1])
    
    
  }
  
  chose.na<-is.na(df1$book)
  sum(chose.na,na.rm = T)
  tail(which(chose.na))
  ###move backwards to table
  lastna.2<-tail(which(chose.na))[6]
  
}

#sum(chose,na.rm = T)


for (k in 1:length(ch.u)){
  set<-subset(df1,df1$h2==ch.u[k]&df1$book==book.n)
  if(length(set$X)!=0){
    h2<-set$h2
    m<-grepl("<h2>",df1$p[df1$h2==h2])
    h2.t<-df1$p[df1$h2==h2][m]
    book.h<-c(h2.t,set$p)
    ch.text<-c(book.h,set$p)
    writeLines(ch.text,paste0(dir.out.html,"/",ben.ns,book.n,"-",ch.u[k],".html"))
    writeLines(ch.text,paste0(dir.gl.htm,"/",ben.ns,book.n,"-",ch.u[k],".html"))
  }
}
#ben13<-df1$p[df1$book==1]
#writeLines(ben13,paste0("expo/benjamin13.2-",book.n,".html"))
#writeLines(ben13,"benjul/benjamin13-1.html")

### to markdown
dir.out.md<-paste0("expo/",ben.ns,book.n,"-md")
dir.create(dir.out.md)
#dir.create("expo/benjul2")

#k<-3
#ch.u
for (k in 1:length(ch.u)){
  set<-subset(df1,df1$h2==ch.u[k]&df1$book==book.n)
  if(length(set$X)!=0){
    h2<-set$h2
    m<-grepl("<h2>",df1$p[df1$h2==h2])
    h2.t<-df1$p[df1$h2==h2][m]
  book.h<-c(h2.t,set$p)
  book.h.c<-paste0(book.h,collapse = " ")
  xhtm<-minimal_html(book.h.c)
  all.h<-xml_find_all(xhtm,"//h2")
  all.p<-xml_find_all(xhtm,"//p")
#  xml_text(all.p)<-gsub("<em>|</em>","*",xml_text(all.p))
  m.h<-grep("<h2>",book.h)
  m.i<-grep("<em>",book.h)
  m.b<-grep("<strong>|</strong>",book.h)
  m.p<-grep("<p>",book.h)
  book.h[m.i]<-gsub("<em>|</em>","*",book.h[m.i])
  book.h[m.b]<-gsub("<strong>|</strong>","**",book.h[m.b])
  #book.h[m.h]<-xml_text("<h2>","## ",book.h[m.h])
  #book.h[m.h]<-gsub("</h2>","",book.h[m.h])
  book.h[m.h]<-paste0("## ",xml_text(all.h))
  book.h[m.p]<-gsub("<p>","",book.h[m.p])
  book.h[m.p]<-gsub("</p>","   ",book.h[m.p])
  
  #book.h[m.i]<-xml_text(all.p)
  writeLines(book.h,paste0(dir.out.md,"/",ben.ns,book.n,"-",ch.u[k],".md"))
  writeLines(book.h,paste0(dir.gl.md,"/",ben.ns,book.n,"-",ch.u[k],".md"))
  }
#  writeLines(set$p,paste0("expo/benjamin13-1/benjamin13-1-",ch.u[k],".html"))
}
return(df1)
}

df1<-read.csv("benjamin.DF.2.csv")
df1$book<-NA
df.files.md<-data.frame(md_1=0,md_2=0,md_3=0,md_4=0,md_5=0)
df.files.htm<-data.frame(htm_1=0,htm_2=0,htm_3=0,htm_4=0,htm_5=0)
df.x<-df1
###THIS LOOP over books
#df.x<-df1 # initiate first table, not necessary
for (k in 1:5){
df.x<-getbook(k)
csv.ns<-paste0("benjamin13.DF-",book.n,".csv")
#write_csv(df1,"benjamin.DF.2.csv")
write_csv(df.x,csv.ns)
f.nr<-list.files()
lookup.md<-paste0("expo/",ben.ns,k,"-md")
f.nr.md<-list.files(lookup.md)
df.files.md[,k]<-length(f.nr.md)
lookup.htm<-paste0("expo/",ben.ns,k,"-htm")
f.nr.htm<-list.files(lookup.htm)
df.files.htm[,k]<-length(f.nr.htm)
df.files.c<-cbind(df.files.md,df.files.htm)
write_csv(df.x,paste0(ben.ns,"DF.csv"))

  cat("files book",k,"written.")
}
#df1$book[1:6]<-5
write_csv(df1,paste0("benjamin13.DF-003.csv"))
datestamp<-13342.1
f.l<-length(df.files.c[,1])
df.files.c$sum<-sum(df.files.c[f.l,1:10])
df.files.c$date<-datestamp
#write_csv(df.files.c[1,],"benjamin13-count.csv")
#df.files.c[2,]<-10
write_csv(df.files.c,"benjamin13-count.csv",append = T)

book.h
getbook(3)
df5<-read_csv("benjamin13.DF-2m.csv")
df1<-df5
### wks.
# now an mdb summary .md
k<-1

md.summary<-array()
for (k in 5:1){
  lookup.md<-paste0("expo/",ben.ns,k,"-md")
  f.nr.md<-list.files(lookup.md)
  f.nr.md
  f.nr.s<-stri_extract_all_regex(stri_extract_all_regex(f.nr.md,"-[0-9]{1,3}\\.md",simplify = T),
                                 "[0-9]{1,3}",simplify = T) # get chapter #
  f.nr.s<-f.nr.s[order(as.double(f.nr.s))]
  f.nr.s
  ###
  f.nr.md.s<-f.nr.md[order(as.double(stri_extract_all_regex(
    stri_extract_all_regex(f.nr.md,"-[0-9]{1,3}\\.md",simplify = T),
                                                            "[0-9]{1,3}",simplify = T)))]
  f.nr.md.s
  ###
  dash<-array()
  c<-1
  c
  for (c in length(f.nr.s):1){
  dash<-append(paste0("  - [chapter",c,"](",f.nr.md.s[c],")"),dash)
  dash 
  }
  ###wks
  dash.t<-paste0("- [buch ",k,"](book",k,"-0.md)")
  dash.c<-c(dash.t,dash)
  dash.c<-dash.c[!is.na(dash.c)]
md.summary<-append(dash.c,md.summary)  
  
  cat("files book",k,"written.")
}
md.summary<-c("## SUMMARY",md.summary)
md.summary<-md.summary[!is.na(md.summary)]

writeLines(md.summary,"expo/SUMMARY.md")


