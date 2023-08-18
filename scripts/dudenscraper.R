#13332.duden check for lexical items
#20230813(15.25)
################
library(stringi)
library(readr)
library(readtext)
library(httr)
library(xml2)

duden.url<-"https://www.duden.de/suchen/dudenonline/"

#13332.from kraken sketch to OCR dataframe
# setwd() where files are
callkraken<-"kraken -f pdf -i grammar_p01.pdf grammar.txt binarize segment -bl ocr -m german_print_best.mlmodel
"
system(callkraken)

#13331.check duden dictionary for misspelled recognitions

oedurl_base<-duden.url
urlq<-""
####
# TERM:
q<-"gratier"

checkitem<-function(q){
  
  body_q<-paste0(urlq,q)
  oedurl_r<-paste0(oedurl_base,body_q)
  req<-httr::GET(oedurl_r)
  x<-httr::content(req,"text")
  oed_html<-read_html(x)
  a<-xml_find_all(oed_html,"//div")
  xpath<-"/html/body/div[3]/div/div[1]/main/div[2]/div/header/div/p" #safari copy. not wks.
  a<-xml_find_all(oed_html,xpath = xpath)
  a<-xml_find_all(oed_html,"//body/div/div/div/main/div/div/header/div/p")
  a1<-xml_text(a)
#  length(resp.true)
  
    print(a1)
}
q<-"vieleicht"
resp.true<-checkitem(q)
length(resp.true)
# if length(resp.true) == 1: no lexical item.

x
a
as<-a
a1



