#20240103(07.42)
#SPUND.corpusLX.stefanowitsch.HA
################################
Q.1<-"Mehl, S. orcid.org/0000-0003-3036-8132 (2018) What we talk about when we talk about corpus frequency: The example of polysemous verbs with light and concrete senses. 
Corpus Linguistics and Linguistic Theory. ISSN 1613-7027 https://doi.org/10.1515/cllt-2017-0039"
R.p23<-"If onomasiological frequency measurements do indeed correlate with elicitation tests, 
potential impact would be immense. Researchers would be able to examine onomasiological frequencies in spoken corpora 
rather than performing elicitation tests. That possibility would facilitate cognitive research into languages and 
varieties around the world, without the necessity of in situ psycholinguistic testing, 
and would also encourage the creation of more spoken corpora. "
R.1<-"https://www.linguistics.ucsb.edu/research/santa-barbara-corpus"
Q.2<-"https://www.linguistics.ucsb.edu/sites/secure.lsit.ucsb.edu.ling.d7/files/sitefiles/research/SBC/SBCorpus.zip"
Q.3<-"https://www.linguistics.ucsb.edu/sites/secure.lsit.ucsb.edu.ling.d7/files/sitefiles/research/SBC/SBCSAE_chat.zip"
library(utils)
getwd()
#setwd("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA")
#tempdir()
#dir.create("data")
sbctemp<-tempfile("SBCtemp.zip")
sbctempdir<-tempdir()
#download.file(Q.2,"SBC.zip")
download.file(Q.2,sbctemp)
#unzip("SBC.zip",exdir = "data")
unzip(sbctemp,exdir = sbctempdir)
#regx<-".([0-9]{1,7}_[0-9]{1,7})."

library(readr)
# # SBC001 <- read_delim("data/TRN/SBC001.trn", 
# #                      delim = "\t", escape_double = FALSE, 
# #                      trim_ws = TRUE,col_names = c("id","spk","text"))
# View(SBC001)
#filestrn<-list.files("data/TRN")
# SBC015 <- read_delim("data/TRN/SBC015.trn", 
#                      delim = "\t", escape_double = FALSE, 
#                      trim_ws = F,col_names = c("id","spk","text"))
# View(SBC014)
sbctrn<-paste0(sbctempdir,"/TRN/")
filestrn<-list.files(sbctrn)
filestrn
#trnlist<-list()
trndf<-data.frame(scb=NA,id=NA,text=NA)
for(k in 1:length(filestrn)){
  cat(k,"\n")
# trnlist[[k]]<-read_delim(paste0("data/TRN/",filestrn[k]), 
#                          delim = "\t", escape_double = FALSE, 
#                          trim_ws = TRUE,col_names = c("id","spk","text"))
trntemp<-read_delim(paste0(sbctrn,filestrn[k]), 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE,col_names = c("id","spk","text"))
l1<-length(trntemp)
trntext<-trntemp[,l1]
colnames(trntext)<-"text"
trntemp.2<-data.frame(scb=k,id=1:length(trntext$text),text=trntext)
  
trndf<-rbind(trndf,trntemp.2)

}
trndf$lfd<-1:length(trndf$scb)
#save(trndf,file = "~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA/data/SCB-df.cpt.RData")
m1<-grep("tak",trndf$text) #take:415,tak:478 obs.
trn.take<-cbind(trndf[m1,],"concrete"=0,"light"=1)
m2<-grep("mak",trndf$text) #take:415,tak:478 obs.
trn.make<-cbind(trndf[m2,],"concrete"=0,"light"=1) #430
m3<-grep("giv",trndf$text) #take:415,tak:478 obs.
trn.give<-cbind(trndf[m3,],"concrete"=0,"light"=1) #235
### wks., wonderful. now annotate for concrete/light use
#trn.make.a<-fix(trn.make)
trn.make.a$lfd<-trn.make$lfd
trndf$lfd<-1:length(trndf$scb)
#save(trn.make.a,file = "~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA/data/make.annotated.RData")
#save(trndf,file = "~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA/data/SCB-df.cpt.RData")
write_csv(trn.make.a,"~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA/data/make.ann.csv")
write_csv(trndf,"~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA/data/scb-raw.csv")
eval1.m<-(trn.make.a$concrete==1)
eval2.m<-(trn.make.a$light==1&trn.make.a$concrete==0)
eval3.m<-(trn.make.a$concrete==0&trn.make.a$light==0|trn.make.a$concrete==-9)
eval1<-sum(eval1.m)
eval2<-sum(eval2.m)
eval3<-sum(eval3.m)
eval4<-length(trn.make.a$scb)-eval3
p.light<-eval2/eval4 #89.4%
p.concrete<-eval1/eval4 #10.6%
eval1+eval2
# load("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA/data/SCB-df.cpt.RData")
#trndf<-trndf[2:length(trndf$scb),]
#trndf$lfd<-1:length(trndf$scb)
library(readr)
library(stringi)
trn.split<-stri_split_boundaries(trndf$text,type="word",simplify = T)
get.mfw<-function(t.array){
  m1<-grepl("[^A-Za-z ]",t.array)
  sum(m1)
  t.array<-t.array[!m1]
  t1<-table(t.array)
  t1<-sort(t1,decreasing = T)
}
df<-trn.split
k<-6
cleandf<-function(df){
  for(k in 2:length(df[,1])){
  t.array<-df[k,]
  m1<-grepl("[^A-Za-z ]",t.array)
  t.array
  sum(m1)
  t.array<-t.array[!m1]
  t.array
  m2<-t.array!=""
  sum(m2)
  t.array
  t.array<-t.array[m2]
  t.array
  m3<-t.array!=" "
  sum(m3)
  t.array
  t.array<-t.array[m3]
  t.array
  length(t.array)
  df[k,]<-NA
  if(length(t.array)>0)
    df[k,1:length(t.array)]<-t.array
  }
  return(df)
}
k
df[3,]
cleantrn<-cleandf(trn.split)
cleantrn[4,]
t.con<-get.mfw(trn.split[trn.make.a$lfd[eval1.m],]) #gets most frequent words for selection
t.con
#t.array<-trn.split[trn.make.a$lfd[eval1.m],]
### PoS tagging
load("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA/data/SCB-df.cpt.RData")
library(udpipe)
library(stringr)
library(stringi)
#udpipe_download_model("english",udpipe_model_repo = "jwijffels/udpipe.models.ud.2.5")
udpipepath<-"~/boxHKW/21S/DH/local/SPUND/corpuslx/english-ewt-ud-2.5-191206.udpipe"
md<-udpipe_load_model(udpipepath)
#an1<-udpipe_annotate(md,cleantrn[7,])                
#an1                      
#an2<-as.data.frame(an1)      
tna<-is.na(trndf$text)
sum(tna)
trndf<-trndf[!tna,]
an3<-udpipe_annotate(md,x=trndf$text,tagger = "default",parser = "none")
#save(an4,file="~/Documents/GitHub/R-essais/SPUND/corpusLX/14015-HA/SCB-df.PoS.RData")
#save(trndf,file="~/Documents/GitHub/R-essais/SPUND/corpusLX/14015-HA/SCB-df.cpt.RData")
an2<-udpipe_annotate(md,x=trndf$text[40001:length(trndf$text)],tagger = "default",parser = "none")

an4<-list(docid=an3$x,pos=an3$conllu)
load("~/boxHKW/21S/DH/local/SPUND/corpuslx/stefanowitsch/HA/data/SCB-df.ann.RData")
an5<-as.data.frame(an3$x,an3$conllu)
an6<-as.data.frame(an2)
an5[5,]
# R crashes
### df too large
### created corpus in sketchengine
library(readr)
dmake<-read.csv("~/Documents/GitHub/R-essais/SPUND/corpusLX/14015-HA/make+objects_of_make.csv",skip = 4) # sketchengine export
library(stringi)
stri_extract_all_regex(dmake$Right,"(.*<coll>.*</coll>.+?[0-9]{1,3}\\.)")
coll.make<-stri_extract_all_regex(dmake$Right,"(.*<coll>.*</coll>.+?[0-9]{1,3}\\.)")
coll.make
dmake.conc<-data.frame(kwic=dmake$KWIC,coll=unlist(coll.make))
dmake.conc$concrete<-1
dmake.conc$light<-1
dmake.conc$synonyme<-NA
dmake.conc$n.alt<-NA
dmake.conc$n.0<-NA
dmake.fix<-fix(dmake.conc)
dmake.fix$left<-dmake$Left
colnames(dmake.fix.2)
dmake.fix.2<-dmake.fix[,c(8,1,2,3,4,5,6,7)]
stri_extract_all_regex(dmake$Left,"((.){30}$)")
dmake.fix.2$left<-unlist(stri_extract_all_regex(dmake$Left,"((.){30}$)"))
dmake.fix.3<-fix(dmake.fix.2)
dmake.fix.3$coll<-unlist(coll.make)
dmake.fix.4<-fix(dmake.fix.3)
m<-dmake.fix.4$light==1
dmake.fix.4$concrete[m]<-1
dmake.fix.4$coll[!m]
save(dmake.fix.4,file = "~/Documents/GitHub/R-essais/SPUND/corpusLX/14015-HA/make.annotated(SkE-coll).RData") # 238 obs.
load("~/Documents/GitHub/R-essais/SPUND/corpusLX/14015-HA/make.annotated.RData") # 430 obs.
#####
trn.make.a$coll<-NA
trn.make.a$synonyme<-NA
trn.make.a$n.alt<-NA
trn.make.a$n.0<-NA
colnames(trn.make.light)
trn.make.a<-trn.make.a[,c(1,2,3,6,4,5,7,8,9)]
trn.make.light<-trn.make.a[trn.make.a$concrete==0&trn.make.a$light==1,] #321 obs
mode(trn.make.light$coll)<-"character" # !!!!important: if not, then fix() will not save string input
mode(trn.make.light$synonyme)<-"character"
make.l.3<-fix(trn.make.light)
save(make.l.4,file = "~/Documents/GitHub/R-essais/SPUND/corpusLX/14015-HA/make.annotated.light.RData") # 238 obs.
m<-make.l.1$concrete==1
sum(m)
make.l.2<-make.l.1[!m,]
make.l.2[!is.na(make.l.2$synonyme),]
mode(make.l.2$coll)<-"character" # !!!!important: if not, then fix() will not save string input
mode(make.l.2$synonyme)<-"character"
make.l.4<-fix(make.l.3)