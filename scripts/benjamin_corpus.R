#13336.corpus essai: benjaminfeldkraft
#20230817(17.04)
################

library(xml2)
library(jsonlite)
library(XML)
library(rvest)
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
