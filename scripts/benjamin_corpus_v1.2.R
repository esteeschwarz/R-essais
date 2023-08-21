#13336.corpus essai: benjaminfeldkraft
#20230817(17.04)
################

library(xml2)
library(jsonlite)
library(XML)
library(rvest)
library(stringi)
library(quanteda)
####
setwd("~/boxHKW/21S/DH/local/SPUND/corpuslx/benjamin")

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


xml_text(h1p[1])


###13341.new essai with regex wrapping
xhtm<-read_html("benjamin13.html",encoding = "UTF-16") # sonderzeichen messed
#write_xml(doc0,"testwrite.html")
#all.top<-xml_find_all(xhtm,"//*")
all.p1<-xml_find_all(xhtm,"//p")
all.h1<-xml_find_all(xhtm,"//h2")

t1<-readLines("benjamin13.html")
t1[21]
length(t1)
t.e<-t1!=""
t1<-t1[t.e]
all.h<-grep("<h2>",t1)
all.p<-grep("<p>",t1)
length(all.h)+length(all.p)
mh<-all.h
#sum(t1=="<br/>")

p.end<-mh-1
p.start<-mh+1
p.start<-c(1,p.start)
t1[p.start[1]]
p.end<-c(p.end,length(p.end))
p.start.h<-c(1,mh)
df<-data.frame(p=t1)
#df$p<-xml_text(all.p1)
df$h2<-1:length(t1)
h<-1
for (h in 1:length(mh)){
  cat ("heading:",h,"\n")
  
  #mp<-grep("p",)
  #k<-h
  lp<-p.start[h]:p.end[h]
  lp<-p.start.h[h]:p.end[h]
lp
  #  lp<-length(lp)
  e<-1
  for (e in 1:length(lp)){
    df$h2[lp]<-h
    cat("paragraph",e,"\n")
  }
  
#  write_xml(hp,paste0("expo/ben_ch_",h,".html"))
  cat("written\n")
}
for (w in 1:length(p.start.h)){
  t2<-df$p[df$h2==w]
  t2.ns<-paste0("expo/benjamin13_k_",w,".html")
  writeLines(t2,t2.ns)
  
  
}
booknr<-1
getbook<-function(booknr){
txx<-paste0("benjaminbuch",booknr,".html")  
ben1<-readLines("benjaminbuch1.html")
ben1<-readLines(txx)

t1<-ben1
t1[21]
length(t1)
t.e<-t1!=""
t1<-t1[t.e]
all.st<-grep("<strong>",t1)
#get all fat headings:
regx<-"<strong>.+?</strong>"
all.h.st<-stri_extract_all_regex(t1,regx,simplify = T)
all.h.st<-gsub("<strong>|</strong>","",all.h.st)
all.h.st<-all.h.st[!is.na(all.h.st)]
all.h.a<-grep("<h2>",df$p)
df$h3<-NA
df$h3[all.h.a]<-gsub("<h2>|</h2>","",df$p[all.h.a])
all.h.m<-df$h3 %in% all.h.st
sum(all.h.m)
df$book[all.h.m]<-booknr
df.x.tok<-tokens(t1,"word")
ltok<-length(df.x.tok)
df.x.tok[20:30]
#tokens_ngrams(tokens(df1$p[24],"word"),5,concatenator = " ")
ngrams.x<-tokens_ngrams(df.x.tok,5,concatenator = " ")
ngrams.x[20:30]
df.1.tok<-tokens(df$p)
df.1.tok[20:30]
ngrams.1<-tokens_ngrams(df.1.tok,5,concatenator = " ")
ngrams.1[20:30]
ngrams.1u<-unlist(ngrams.1)
ngrams.xu<-unlist(ngrams.x)
ngrams.1s<-sample(ngrams.1u,length(ngrams.1u)/4)
m.tok<-ngrams.1s[1:length(ngrams.1s)] %in% ngrams.xu[1:length(ngrams.xu)]
m.tok<-ngrams.1u[1:length(ngrams.1u)] %in% ngrams.xu[1:length(ngrams.xu)]

sum(m.tok)
which(m.tok)
ngrams.1u[m.tok]
m.tok.2<-array()
k<-23
for (k in 1:length(ngrams.1u)){
m.tok.2[k]<-sum(ngrams.1u[k]%in%ngrams.xu[1:length(ngrams.xu)])  
print(k)
if(m.tok.2[k]!=0)
  print(ngrams.1[k])
}
m.tok.2<-m.tok.2==1
ngrams.1u[1731]
ngrams.1u[1731]%in%ngrams.xu
ngrams.1u[m.tok.2]
m.tok.3<-ngrams.1u[m.tok.2]
k<-300
df1$book<-NA
#df.x.tok<-tokens(t1,"word")
ltok<-length(df.x.tok)
df.x.tok[20:30]
#tokens_ngrams(tokens(df1$p[24],"word"),5,concatenator = " ")
ngrams.x<-tokens_ngrams(df.x.tok,8,concatenator = " ")
ngrams.x[10]
ngrams.xu<-unlist(ngrams.x)
#seq(from = 1, to = 100, by = 2)[1] 
ngrams.s<-seq(from = 1, to = length(ngrams.xu), by = 8)
ngrams.s<-ngrams.xu[ngrams.s]
ngrams.c<-paste0(tokens_chunk(df.x.tok,5),collapse = " ")
ngrams.c[21]
k<-3302
df1$book<-NA
for (k in 1:length(ngrams.s)){
  regx<-gsub("(\\(|\\)|\\[|\\]|\\{|\\})","\\//1",ngrams.s[k])
#  regx<-gsub("(\\(|\\)|\\[|\\])","\\//1",ngrams.xu[k])
  m<-grep(regx,df$p)
  print(k)
  print(m)
  df1$book[m]<-1
  m<-0
}
regx
df1$p[m]
write.csv(df1,"benjamin.DF.csv")
df1$h2[df1$book!=1]
r<-"aber in der Vergangenheit weiter und versuchen"
grep(r,ngrams.1u)
ngrams.1u[20:30]
grep(r,df1$p)

grep("anders kommen",ngrams.1u)
ngrams.1u[m.tok.2]
ngrams.xu[23:40]
ngrams.xu[23]%in%ngrams.1u
sum(m.tok.2)
sum(ngrams.1%in%ngrams.1u[m.tok])
return(df)
}

df1<-getbook(1)
df.tok<-tokens(df1$p,"word")
tokens_ngrams(df.tok,5,concatenator = " ")
tokens_ngrams(tokens(df1$p[24],"word"),5,concatenator = " ")

all.p<-grep("<p>",t1)
length(all.h)+length(all.p)
mh<-all.h


